(in-package :echorepl)

(defvar *clip-store* nil)
(defvar *score* nil)

;; naming stuff

(defun by-name (name)
  (find name *clip-store*
	:test (lambda (name clip)
		(eq name (name clip)))))

(defun clip-names-in-score ()
  (let ((output nil))
    (labels ((trawl (score)
	       (cond
		 ((by-name score)
		  (push score output))
		 ((atom score))
		 (t
		  (trawl (car score))
		  (trawl (cdr score))))))
      (trawl *score*)
      (delete-duplicates output))))

(defun rename (new-name &optional (old-name (name (car *clip-store*))))
  (labels
      ((rename-in-score (score)
	 (cond
	   ((atom score)
	    (if (eq score old-name)
		new-name
		score))
	   (t
	    (cons (rename-in-score (car score))
		  (rename-in-score (cdr score)))))))
    (let ((old-names (mapcar #'name *clip-store*)))
      (unless (find new-name old-names)
	(setf *score* (rename-in-score *score*))
	(loop for clip in *clip-store* do
	     (if (eq (name clip) old-name)
		 (setf (name clip) new-name)))))))

(defun string-to-keyword (string)
  (if (eq (elt string 0) ":")
      (read-from-string string)
      (read-from-string (concatenate 'string ":" string))))

(defun rename-for-slime (old-name new-name)
  (rename (string-to-keyword new-name)
	  (string-to-keyword old-name)))

;; clip-playing functions

(defun play-null (&rest args)
  "Don't do anything."
  (declare (optimize (speed 3) (space 0) (safety 0)
		     (debug 0) (compilation-speed 0))
	   (ignore args)))

(defun play-fun (clip)
  (declare (clip clip))
  "Return a function that plays CLIP once, and returns CLIP's modulus"
  (let ((tape (tape clip))
	(offset (offset clip))
	(modulus (modulus clip))
	(elapsed (number-moment 0))
	(pos-a 0)
	(pos-b 0)
	(gain-b 0.0))
    (declare (moment elapsed)
	     (fixnum pos-a pos-b)
	     (single-float gain-b))
    (lambda (dst i time start gain)
      (declare (single-float gain)
	       (optimize (speed 3) (space 0) (safety 0)
			 (debug 0) (compilation-speed 0)))
      (setf elapsed (moment- time (moment+ start offset))
	    pos-a (frame elapsed)
	    pos-b (1+ pos-a)
	    gain-b (* gain (fraction elapsed)))
      (pos-play dst i tape pos-a (- gain gain-b))
      (pos-play dst i tape pos-b gain-b)
      modulus)))

(defun cycle (fun)
  "Return a function that plays FUN repeatedly"
  (let ((modulus 0)
	(prev-start (number-moment 0))
	(this-start (number-moment 0))
	(next-start (number-moment 0))
	(first-run t))
    (declare (function fun)
	     (fixnum modulus)
	     (moment prev-start this-start next-start)
	     (boolean first-run))
    (lambda (dst i time start gain)
      (declare (optimize (speed 3) (space 0) (safety 0)
			 (debug 0) (compilation-speed 0)))
      (cond
	(first-run
	 (setf modulus (funcall fun dst i time start gain) ;; set start times
	       prev-start (copy-structure start)
	       this-start (moment (+ (frame prev-start) modulus)
				  (fraction prev-start))
	       next-start (moment (+ (frame this-start) modulus)
				  (fraction this-start))
	       first-run nil))
	((moment< next-start time)
	 (shiftf (frame prev-start) ;; shift start-times forward
		 (frame this-start)
		 (frame next-start)
		 (+ (frame next-start) modulus)))
	((moment< time this-start)
	 (shiftf (frame next-start) ;; shift start-times backwards
		 (frame this-start)
		 (frame prev-start)
		 (- (frame prev-start) modulus))))
      (funcall fun dst i time prev-start gain)
      (funcall fun dst i time this-start gain)
      (funcall fun dst i time next-start gain))))

(defun series (&rest play-funs)
  "Return a function that plays the functions in PLAY-FUN sequentially."
  (let ((sub-start (number-moment 0)))
    (declare (moment sub-start))
    (lambda (dst i time start gain)
      (declare (moment time start)
	       (single-float gain)
	       (optimize (speed 3) (space 0) (safety 0)
			 (debug 0) (compilation-speed 0)))
      (setf (frame sub-start) (frame start)
	    (fraction sub-start) (fraction start))
      (loop
	 for fun in play-funs do
	   (incf (frame sub-start)
		 (the fixnum
		      (funcall (the function fun) dst i time sub-start gain)))
	 finally
	   (return (the fixnum (- (frame sub-start) (frame start))))))))

(defun repeat (times fun)
  "Return a function that plays FUN a number of TIMES in a row."
  (apply #'series (make-list times :initial-element fun)))

;; Volume Changing Functions

(defun db-gain (decibel)
  "Change decibels to a simple multiplier (100db = 1.0)"
  (coerce (expt 10
		(/ (- decibel 100)
		   20))
	  'single-float))

(defun gain (db fun)
  "Return a function that plays FUN, with a gain of DB (in decibels)."
  (declare (real db)
	   (function fun))
  (let ((new-gain (db-gain db)))
    (declare (single-float new-gain))
    (lambda (dst i time start old-gain)
      (declare (moment time start)
	       (single-float old-gain)
	       (optimize (speed 3) (space 0) (safety 0)
			 (debug 0) (compilation-speed 0)))
      (funcall fun dst i time start (* new-gain old-gain)))))

(defun mute (fun)
  "Return a function that doesn't play FUN, but passes on its timing info."
  (declare (function fun))
  (lambda (dst i time start gain)
    (declare (moment time start)
	     (ignore gain)
	     (optimize (speed 3) (space 0) (safety 0)
		       (debug 0) (compilation-speed 0)))
    (funcall fun dst i time start 0.0)))

(defun score-modulus (score)
  "Find how long SCORE should take to play."
  (cond
    ((by-name score)
     (modulus (by-name score)))
    ((atom score)
     0)
    (t
     (case (car score)
       (cycle
	0)
       (series
	(apply #'+ (mapcar (lambda (score) (score-modulus score))
			   (cdr score))))
       (repeat
	(score-modulus (cons 'series (make-list (cadr score)
						:initial-element (caddr score)))))
       (otherwise
	(apply #'max (mapcar (lambda (score) (score-modulus score))
			     score)))))))

(defun replace-names-with-playfuns (score)
  (cond
    ((by-name score)
     (play-fun (by-name score)))
    ((atom score)
     score)
    (t
     (cons (replace-names-with-playfuns (car score))
	   (replace-names-with-playfuns (cdr score))))))

(defun compile-score ()
  "Return a function that will CYCLE each element of SCORE altogether."
  (let ((funs (mapcar (lambda (track) (eval (list 'cycle track)))
		      (replace-names-with-playfuns *score*))))
    (lambda (&rest args)
      (declare (optimize (speed 3) (space 0) (safety 0)
			 (debug 0) (compilation-speed 0)))
      (loop for fun in funs do
	   (apply (the function fun) args)))))
