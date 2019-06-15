(in-package :echorepl)

;; half-splice and fudge-factor,
;; i.e. extra recording time at beginning and end of clips

(defparameter *default-half-splice* (round (/ *sample-rate* 16)))
(defparameter *default-fudge-factor* *sample-rate*)

(defun set-edge-space-defaults ()
  (defparameter *default-half-splice* (round (/ *sample-rate* 16)))
  (defparameter *default-fudge-factor* *sample-rate*))

(set-edge-space-defaults)

;; "tape" is the medium of recording:
;; here, a C array of samples, plus information about where the important sound begins

(defcstruct tape
  "the medium of audio recording"
  ;; the samples
  (tape-len nframes-t)
  (samples :pointer)
  
  ;; loop and splice positions
  (clip-start-pos nframes-t)
  (fadein-start-pos nframes-t)
  (fadein-end-pos nframes-t)

  (fadeout-start-pos nframes-t)
  (fadeout-end-pos nframes-t)
  (splice sample-t))

(defcfun "create_tape" :pointer
  (tape-len nframes-t))

(defcfun "delete_tape" :void
  (tape :pointer))

(defun pos-play (dst i tape pos gain)
  (declare (fixnum pos i)
	   (single-float gain)
	   (optimize (speed 3) (space 0) (safety 0)
		     (debug 0) (compilation-speed 0)))
  (foreign-funcall "pos_play"
		   :pointer dst nframes-t i
		   :pointer tape :long pos :float gain :void))

(defun raw-copy (dst i tape moment)
  (declare (fixnum i)
	   (moment moment)
	   (optimize (speed 3) (space 0) (safety 0)
		     (debug 0) (compilation-speed 0)))
  (foreign-funcall "raw_copy"
		   :pointer dst nframes-t i
		   :pointer tape
		   :long (frame moment) :float (fraction moment)))

;; a "clip" is tape plus some timing information

(defun new-name (prefix)
  (intern (symbol-name (gensym prefix)) "KEYWORD"))

(defstruct (clip (:conc-name nil)
		 (:print-function (lambda (clip stream depth)
				    (declare (ignore depth))
				    (format stream "#<CLIP :~a>" (name clip)))))
  (name (new-name "CLIP") :type symbol)

  ;; useful for file save
  (sample-rate *sample-rate* :type fixnum)

  ;; timing
  (clip-len 0 :type fixnum)
  (half-splice *default-half-splice* :type fixnum)
  (fudge-factor *default-fudge-factor* :type fixnum)
  
  ;; tape
  tape
  
  ;; synchronization
  (offset (number-moment 0) :type moment)
  (modulus 0 :type fixnum)
  (parent-modulus 0 :type fixnum))

;; accessing tape values

(defun tape-len (clip)
  (foreign-slot-value (tape clip) '(:struct tape) 'tape-len))

(defun samples (clip)
  (foreign-slot-value (tape clip) '(:struct tape) 'samples))

(defmacro getter-and-setter (name)
  (let ((setter (intern (concatenate 'string "set-" (symbol-name name)))))
    `(progn
       (defun ,name (clip)
	 (foreign-slot-value (tape clip) '(:struct tape) ',name))
       (defun ,setter (clip val)
	 (setf
	  (foreign-slot-value (tape clip) '(:struct tape) ',name)
	  val))
       (defsetf ,name ,setter))))

(getter-and-setter clip-start-pos)
(getter-and-setter fadein-start-pos)
(getter-and-setter fadein-end-pos)
(getter-and-setter fadeout-start-pos)
(getter-and-setter fadeout-end-pos)
(getter-and-setter splice)

;; clip basic timing and position setup

(defun set-start-pos (clip &optional start-pos)
  (setf
   (clip-start-pos clip) (or start-pos
			     (+ (half-splice clip)
				(fudge-factor clip)))
   (fadein-start-pos clip) (- (clip-start-pos clip)
			      (half-splice clip))
   (fadein-end-pos clip) (+ (clip-start-pos clip)
			    (half-splice clip))))

(defun set-modulus (clip)
  (setf (modulus clip)
	(if (zerop (parent-modulus clip))
	    (if (zerop (clip-len clip))
		1
		(clip-len clip))
	    (let* ((max-modulus (* (ceiling (clip-len clip) (parent-modulus clip))
				   (parent-modulus clip)))
		   (smaller-modulus (- max-modulus (parent-modulus clip))))
	      (if (< (abs (- smaller-modulus (clip-len clip)))
		     (fudge-factor clip))
		  smaller-modulus
		  max-modulus)))))

(defun set-end-pos (clip)
  (let ((end-pos (+ (clip-start-pos clip)
		    (if (< (abs (- (modulus clip) (clip-len clip)))
			   (fudge-factor clip))
			(modulus clip)
			(clip-len clip)))))
    (setf
     (fadeout-start-pos clip) (- end-pos (half-splice clip))
     (fadeout-end-pos clip) (+ end-pos (half-splice clip)))))

(defun clip-setup (clip)
  "Infer the contents of various slots in CLIP"
  (setf
   (tape clip) (create-tape (+ (clip-len clip) (* 2 (+ (half-splice clip)
						       (fudge-factor clip))))))
  (set-modulus clip)
  (set-start-pos clip)
  (set-end-pos clip)
  (setf (splice clip) (coerce (* (half-splice clip) 2) 'single-float))
  (let ((tape (tape clip)))
    (finalize clip
	      (lambda () (delete-tape tape)))))

(defun empty-clip ()
  (let ((clip (make-clip)))
    (clip-setup clip)
    clip))

;; create clip

(defun create-clip (input-clip
		    start
		    end
		    parent-modulus)
  (declare (clip input-clip)
	   (moment start end)
	   (fixnum parent-modulus)
	   (optimize (speed 3) (space 0) (safety 0)
		     (debug 0) (compilation-speed 0)))
  (let* ((edge-space (+ (the fixnum *default-half-splice*)
			(the fixnum *default-fudge-factor*)))
	 (edge-time (number-moment edge-space)))
    (declare (fixnum edge-space)
	     (moment edge-time))
    (if (moment< start end)
	;; clock is going forward
	(let*
	    ((clip-start (moment- start edge-time))
	     (clip-end (moment+ end edge-time))
	     (moment-length (moment- end start))
	     (clip-len (+ (frame moment-length)
			  (if (< (fraction moment-length) 0.5) 0 1)))
	     (first-chunk (+ clip-len edge-space))
	     (new-clip (make-clip :clip-len clip-len
				  :parent-modulus parent-modulus)))
	  (declare (fixnum clip-len first-chunk))
	  (clip-setup new-clip)
	  (let ((dst (samples new-clip))
		(tape (tape input-clip)))
	    (loop for i fixnum below first-chunk do
		 (progn
		   (raw-copy dst i tape clip-start)
		   (incf (frame clip-start))))
	    (make-thread
	     (lambda ()
	       (loop while (moment< (now) clip-end) do
		    (sleep 0.1))
	       (loop for i fixnum from first-chunk below (tape-len new-clip) do
		    (progn
		      (raw-copy dst i tape clip-start)
		      (incf (frame clip-start)))))))
	  new-clip)
	;; otherwise clock is going backwards
	(let* ((end (copy-moment end))
	       (clip-start (moment- end edge-time))
	       (clip-end (moment+ start edge-time))
	       (moment-len (moment- start end))
	       (clip-len (+ (frame moment-len)
			    (if (< (fraction moment-len) 0.5) 0 1)))
	       (new-clip (make-clip :clip-len clip-len
				    :parent-modulus parent-modulus)))
	  (clip-setup new-clip)
	  (let ((dst (samples new-clip))
		(tape (tape input-clip)))
	    (loop
	       for i fixnum from edge-space below (tape-len new-clip) do
		 (progn
		   (raw-copy dst i tape end)
		   (incf (frame end))))
	    (make-thread
	     (lambda ()
	       (loop while (moment< clip-end (now)) do
		    (sleep 0.1))
	       (loop for i fixnum below edge-space do
		    (progn
		      (raw-copy dst i tape clip-start)
		      (incf (frame clip-start)))))))
	  new-clip))))
