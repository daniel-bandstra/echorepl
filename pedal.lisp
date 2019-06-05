(in-package :echorepl)

;;; C Functions

;; opening and closing the pedal

(defcfun ("open_pedal" internal-open-pedal) :int
  (portname :string))

(defcfun ("close_pedal" internal-close-pedal) :void
  (internal-pedal :int))

;; output and input

(defcfun ("pedal_color" internal-pedal-color) :int
  (internal-pedal :int)
  (red :int)
  (green :int)
  (blue :int))

(defcfun ("read_pedal" internal-read-pedal) :int
  (internal-pedal :int)
  (pedal-event :pointer))

;; pedal event struct

(defcstruct pedal-event
  (event :char)
  (time :uint64))

;;; Pedal Functions

(let ((internal-pedal 0)
      (internal-red 0)
      (internal-green 0)
      (internal-blue 0)
      (pedal-is-open nil)
      (write-lock (make-lock "Pedal Write"))
      (event (foreign-alloc '(:struct pedal-event))))

  (declare (fixnum internal-pedal)
	   (type (integer 0 128) internal-red internal-green internal-blue)
	   (boolean pedal-is-open))

  ;; open and close
  
  (defun pedal-close ()
    (if pedal-is-open
	(internal-close-pedal internal-pedal))
    (setf pedal-is-open nil))
  
  (defun pedal-open (portname)
    (declare (string portname))
    (pedal-close)
    (setf internal-pedal (internal-open-pedal portname))
    (if (>= internal-pedal 0)
	(setf pedal-is-open t)))

  ;; color write
  
  (defun pedal-set-color (red green blue)
    (setf internal-red red
	  internal-green green
	  internal-blue blue))

  (defun pedal-refresh-color ()
    (if pedal-is-open
	(with-lock-held (write-lock)
	  (internal-pedal-color internal-pedal
				internal-red
				internal-green
				internal-blue))))

  (defun pedal-color (red green blue)
    (setf internal-red red
	  internal-green green
	  internal-blue blue)
    (pedal-refresh-color))

  (defun pedal-blink (red green blue)
    (declare (type (integer 0 128) red green blue))
    (if pedal-is-open
	(progn
	  (with-lock-held (write-lock)
	    (internal-pedal-color internal-pedal red green blue))
	  (make-thread (lambda ()
			 (sleep 0.1)
			 (pedal-refresh-color))))))
  
  ;; Reading pedal events
  ;; (first, mung pedal_flags.h for keywords)
  
  (let ((flags nil))
    (with-open-file (str
		     (asdf:system-relative-pathname 'echorepl
						    "pedal/pedal/pedal_flags.h")
		     :direction :input)
      (do ((l (read-line str nil :eof) (read-line str nil :eof)))
	  ((or (eq l :eof)
	       (search "enum Pedal_Flags {" l))))
      (do ((l (read-line str nil :eof) (read-line str nil :eof)))
	  ((or (eq l :eof)
	       (search "};" l)))
	(setf flags (append
		     flags
		     (list (let ((comma (position #\, l)))
			     (intern
			      (string-upcase
			       (substitute #\- #\_
					   (string-trim '(#\Space #\Tab)
							(if comma
							    (subseq l 0 comma)
							    l))))
			      "KEYWORD")))))))
    (defun pedal-read ()
      (declare (optimize (speed 3) (space 0) (safety 0)
			 (debug 0) (compilation-speed 0)))
      (if pedal-is-open
	  (progn
	    (internal-read-pedal internal-pedal event)
	    (values (nth (foreign-slot-value event
					     '(:struct pedal-event)
					     'event)
			 flags)
		    (usecs-moment (the fixnum
				       (foreign-slot-value event
							   '(:struct pedal-event)
							   'time)))))
	  (progn (sleep 1)
		 (values :nothing (number-moment 0)))))))

;; try to open the pedal
(pedal-open "/dev/ttyUSB0")
