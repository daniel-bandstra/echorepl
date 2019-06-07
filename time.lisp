(in-package :echorepl)

;; Echorepl keeps track of its own time as a series of moments.
;; The MOMENT struct has a fixnum and a fractional part,
;; because really big FLOATs don't count accurately

(defstruct (moment (:constructor moment (frame fraction))
		   (:conc-name nil))
  (frame 0 :type fixnum)
  (fraction 0.0 :type (single-float 0.0 1.0)))

(declaim (ftype (function (moment moment) moment) moment+ moment-)
	 (ftype (function (moment moment) boolean) moment<)
	 (inline moment+ moment- moment<))

(defun moment+ (a b)
  "Add two moments"
  (declare (moment a b)
	   (optimize (speed 3) (space 0) (safety 0) (debug 1) (compilation-speed 0)))
  (multiple-value-bind (add-frame new-fraction) (floor (+ (fraction a)
							  (fraction b)))
    (declare (fixnum add-frame)
	     (type (single-float 0.0 1.0) new-fraction))
    (moment (the fixnum (+ (frame a)
			   (frame b)
			   add-frame))
	    new-fraction)))

(defun moment- (a b)
  "Subtract B from A"
  (declare (moment a b)
	   (optimize (speed 3) (space 0) (safety 0) (debug 1) (compilation-speed 0)))
  (multiple-value-bind (add-frame new-fraction) (floor (- (fraction a)
							  (fraction b)))
    (declare (fixnum add-frame)
	     (type (single-float 0.0 1.0) new-fraction))
    (moment (the fixnum (+ (- (frame a)
			      (frame b))
			   add-frame))
	    new-fraction)))

(defun moment< (a b)
  "Tell if A is before B"
  (declare (moment a b)
	   (optimize (speed 3) (space 0) (safety 0) (debug 1) (compilation-speed 0)))
  (or (< (frame a) (frame b))
      (and (= (frame a) (frame b))
	   (< (fraction a) (fraction b)))))

(defun number-moment (number)
  "Make a moment out of any real number"
  (declare (real number))
  (if (integerp number)
      (moment number 0.0)
      (multiple-value-bind (frame fraction) (floor (coerce number 'single-float))
	(moment frame fraction))))

;; in what follows, 'frame' refers to frame-time as tracked by Jack.

(let* ((latency (+ *latency* *jack-buffer-size*))
       (now (number-moment -1))

       ;; rate
       (rate (number-moment 1))
       (reverse nil)
       
       ;; translating jack frame to time
       (frame-moment-length (* latency 2))
       (frame-moment-array (make-array frame-moment-length
				       :initial-element (number-moment 0)
				       :element-type 'moment)))
  
  (declare (moment now rate)
	   (boolean reverse)
	   (type (integer 0 #.(1- (expt 2 32)))
		 frame-moment-length))
  
  (defun reset-tick ()
    (setf now (number-moment -1)
	  rate (number-moment 1)
	  reverse nil
	  latency (+ *latency* *jack-buffer-size*)
	  frame-moment-length (* latency 2)
	  frame-moment-array (make-array frame-moment-length
					 :initial-element (number-moment 0)
					 :element-type 'moment))
    t)
  
  (defun tick (frame)
    (declare (fixnum frame)
	     (optimize (speed 3) (space 0) (safety 0)
		       (debug 0) (compilation-speed 0)))
    
    ;; update and return the time
    (let ((pos (mod frame frame-moment-length)))
      (declare (fixnum pos))
      (setf now (if reverse
		    (moment- now rate)
		    (moment+ now rate))
	    (aref frame-moment-array pos) now)))

  (defun reverse-time ()
    (setf reverse (not reverse)))

  (defun frame-moment (frame)
    (declare (fixnum frame))
    "Translate FRAME to a moment."
    (let* ((frame-ago (logand (- frame latency)
			      #.(1- (expt 2 32))))
	   (pos (mod frame-ago frame-moment-length)))
      (declare (fixnum frame-ago pos))
      (aref frame-moment-array pos)))

  (defun time-rate (&optional new-rate)
    (if new-rate
	(setf rate (number-moment (abs new-rate)))
	rate)))

;; getting rather accurate moments

(defun usecs-moment (usecs)
  "Return the precise moment at USECS"
  (if (jack-running)
      (let* ((usecs-frame (usecs-frame usecs))
	     (frame-usecs (frame-usecs usecs-frame))
	     (pre-frame (if (< usecs frame-usecs)
			    (logand (1- usecs-frame)
				    #.(1- (expt 2 32)))
			    usecs-frame))
	     (post-frame (logand (1+ pre-frame)
				 #.(1- (expt 2 32))))
	     (pre-usecs (frame-usecs pre-frame))
	     (post-usecs (frame-usecs post-frame))
	     (fraction (/ (- usecs pre-usecs)
			  (- post-usecs pre-usecs)))
	     (pre-moment (frame-moment pre-frame))
	     (t-diff (moment- (frame-moment post-frame)
			      pre-moment))
	     (add-moment (number-moment (* fraction
					   (+ (frame t-diff)
					      (fraction t-diff))))))
	(moment+ pre-moment add-moment))
      (number-moment 0)))

(defun now-moment ()
  (usecs-moment (usecs)))
