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
  (splice sample-t)

  ;; for play functions
  (offset (:struct c-moment))
  (modulus nframes-t)
  )

(defcfun "create_tape" :pointer
  (tape-len nframes-t))

(defcfun "delete_tape" :void
  (tape :pointer))

(defun pos-play (dst index tape pos gain)
  (declare (fixnum index pos)
	   (single-float gain)
	   (optimize (speed 3) (space 0) (safety 0)
		     (debug 0) (compilation-speed 0)))
  (foreign-funcall "pos_play"
		   :pointer dst :unsigned-int index
		   :pointer tape :long pos :float gain :void))

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
  (tape (null-pointer)) ;; a pointer to a struct
  
  ;; synchronization
  (parent-modulus 0 :type fixnum))

;; accessing tape values

(defun tape-len (clip)
  (foreign-slot-value (tape clip) '(:struct tape) 'tape-len))

(defun samples (clip)
  (foreign-slot-value (tape clip) '(:struct tape) 'samples))

(defun offset (clip)
  (let  ((c-offset (foreign-slot-value (tape clip) '(:struct tape) 'offset)))
    (moment (getf c-offset 'frame)
	    (getf c-offset 'fraction))))

(defun set-offset (clip moment)
  (let ((m-ptr (foreign-slot-pointer (tape clip) '(:struct tape) 'offset)))
    (setf (foreign-slot-value m-ptr '(:struct c-moment) 'frame)
	  (frame moment)
	  (foreign-slot-value m-ptr '(:struct c-moment) 'fraction)
	  (fraction moment)))
  moment)

(defsetf offset set-offset)

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
(getter-and-setter modulus)

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

(defun input-play-fun (clip)
  (declare (clip clip))
  (let ((tape (tape clip))
	(tape-len (tape-len clip)))
    (declare (fixnum tape-len))
    (lambda (dst index time)
      (declare (fixnum index)
	       (moment time)
	       (optimize (speed 3) (space 0) (safety 0)
			 (debug 0) (compilation-speed 0)))
      (let ((pos-a (the fixnum (mod (frame time) tape-len)))
	    (pos-b (the fixnum (mod (1+ (frame time)) tape-len))))
	(pos-play dst index tape pos-a (- 1.0 (fraction time)))
	(pos-play dst index tape pos-b (fraction time))))))

(defun create-clip (input-clip
		    start
		    end
		    parent-modulus)
  (declare (clip input-clip)
	   (moment start end)
	   (fixnum parent-modulus))
  (if (moment< start end)
      ;; clock is going forward
      (let* ((edge-time (number-moment (+ *default-fudge-factor*
					  *default-half-splice*)))
	     (clip-start (moment- start edge-time))
	     (moment-len (moment- end start))
	     (clip-len (+ (frame moment-len)
			  (if (< (fraction moment-len) 0.5) 0 1)))
	     (first-chunk (+ clip-len
			     *default-half-splice*
			     *default-fudge-factor*))
	     (new-clip (make-clip :clip-len clip-len
				  :parent-modulus parent-modulus)))
	(clip-setup new-clip)
	(let ((input-play-fun (input-play-fun input-clip))
	      (dst (samples new-clip)))
	  (loop for i fixnum below first-chunk do
	       (progn
		 (funcall input-play-fun dst i clip-start)
		 (incf (frame clip-start))))
	  (make-thread
	   (lambda ()
	     (sleep (/ first-chunk *sample-rate*))
	     (loop for i fixnum from first-chunk below (tape-len new-clip) do
		  (progn
		    (funcall input-play-fun dst i clip-start)
		    (incf (frame clip-start)))))))
	new-clip)
      ;; clock is going backwards
      (let* ((end (copy-moment end))
	     (edge-time (number-moment (+ *default-fudge-factor*
						   *default-half-splice*)))
	     (clip-start (moment- end edge-time))
	     (moment-len (moment- start end))
	     (clip-len (+ (frame moment-len)
			  (if (< (fraction moment-len) 0.5) 0 1)))
	     (first-spot (+  *default-half-splice*
			     *default-fudge-factor*))
	     (new-clip (make-clip :clip-len clip-len
				  :parent-modulus parent-modulus)))
	(clip-setup new-clip)
	(let ((input-play-fun (input-play-fun input-clip))
	      (dst (samples new-clip)))
	  (loop
	     for i fixnum from first-spot below (tape-len new-clip) do
	       (progn
		 (funcall input-play-fun dst i end)
		 (incf (frame end))))
	  (make-thread
	   (lambda ()
	     (sleep (/ (+ first-spot clip-len) *sample-rate*))
	     (loop for i fixnum below first-spot do
		  (progn
		    (funcall input-play-fun dst i clip-start)
		    (incf (frame clip-start)))))))
	new-clip)))
