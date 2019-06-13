(in-package :echorepl)

;; clip naming stuff

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

;; A link is one in a chain of structs that play various clips.
;; Start/end times include splices. All times are normalized, such that the start
;; of the clip is at (number-moment 0)

;; In what follows "link" is a link, "chain" is a link that may or may not have a
;; next-link

(defstruct (link (:conc-name nil)
		 (:constructor internal-make-link))
  (clip (empty-clip)
	:type clip)

  (this-offset (number-moment 0)
	       :type moment)
  (this-start (number-moment 0)
	      :type moment)
  (this-end (number-moment 0)
	    :type moment)
  (this-modulus 0
		:type fixnum)

  next-link
  (next-start (number-moment 0)
	      :type moment)
  (next-modulus 0
		:type fixnum)

  prev-link
  (prev-end (number-moment 0)
	    :type moment)
  (prev-modulus 0
		:type fixnum)

  (set-mute nil
	    :type boolean)
  (set-gain 1.0
	    :type single-float))

(defun make-link (clip)
  (internal-make-link :clip clip
		      :this-offset (offset clip)
		      :this-start (moment+ (offset clip)
					   (number-moment (- (fadein-start-pos clip)
							     (clip-start-pos clip))))
		      :this-end (moment+ (offset clip)
					 (number-moment (- (fadeout-end-pos clip)
							   (clip-start-pos clip))))
		      :this-modulus (modulus clip)))

;; Arranging links and chains

(defun chain-links (a b)
  "Set link B to play after link A"
  (setf (next-link a) b
	(next-start a) (moment+ (this-start b)
				(number-moment (this-modulus a)))
	(next-modulus a) (this-modulus b)
	(prev-link b) a
	(prev-end b) (moment- (this-end a)
			      (number-moment (this-modulus a)))
	(prev-modulus b) (this-modulus a))
  a)

(defun last-link (chain)
  "Return the last link in CHAIN"
  (if (null (next-link chain))
      chain
      (last-link (next-link chain))))

(defun cycle (chain)
  "Make CHAIN circular"
  (chain-links (last-link chain) chain)
  chain)

(defun series (&rest chains)
  "Make a number of CHAINS into one CHAIN"
  (if (cdr chains)
      (chain-links (car chains)
		   (apply #'series (cdr chains)))
      (car chains)))

(defun copy-chain (chain)
  "Make a chain of copies of the links in CHAIN"
  (if (next-link chain)
      (chain-links (copy-link chain)
		   (copy-chain (next-link chain)))
      (copy-link chain)))

(defun repeat (times chain)
  "Make a chain of CHAIN a number of TIMES in a row"
  (apply #'series
   (loop for i below times
      collect (copy-chain chain))))

;; Playing links and chains

(let ((elapsed (number-moment 0))
      (gain-b 0.0))
  (defun play-link (dst i link moment gain)
    "Play LINK"
    (declare (optimize (speed 3) (space 0) (safety 0)
		       (debug 1) (compilation-speed 0))
	     (single-float gain))
    (unless (set-mute link)
      (if (and (moment< (this-start link) moment)
	       (moment< moment (this-end link)))
	  (progn
	    (setf elapsed (moment- moment (this-offset link))
		  gain-b (* gain (fraction elapsed)))
	    (pos-play dst i (tape (clip link))
		      (frame elapsed) (* (set-gain link)
					 (- gain gain-b)))
	    (pos-play dst i (tape (clip link))
		      (the fixnum (1+ (frame elapsed)))
		      (* (set-gain link) gain-b)))))))

(defun play-chain (dst i chain moment gain)
  "Play CHAIN"
  (declare (optimize (speed 3) (space 0) (safety 0)
		     (debug 1) (compilation-speed 0)))
  (play-link dst i chain moment gain)
  (cond
    ((and (next-link chain)
	  (moment< (next-start chain) moment))
     (decf (frame moment) (this-modulus chain))
     (play-link dst i (next-link chain) moment gain))
    ((and (prev-link chain)
	  (moment< moment (prev-end chain)))
     (incf (frame moment) (prev-modulus chain))
     (play-link dst i (prev-link chain) moment gain))))

(defun play-fun (chain)
  "Return a function to play CHAIN"
  (let ((first-run t)
	(this-start (number-moment 0))
	(next-start (number-moment 0)))
    (lambda (dst i time first-start gain)
      (declare (optimize (speed 3) (space 0) (safety 0)
			 (debug 0) (compilation-speed 0)))
      (if first-run
	  (setf this-start (copy-moment first-start)
		next-start (moment+ first-start
				    (number-moment (this-modulus chain)))
		first-run nil))
      (play-chain dst i chain (moment- time this-start) gain)
      (cond
	((and (prev-link chain)
	      (moment< time this-start))
	 (decf (frame next-start) (this-modulus chain))
	 (decf (frame this-start) (prev-modulus chain))
	 (setf chain (prev-link chain)))
	((and (next-link chain)
	      (moment< next-start time))
	 (incf (frame next-start) (next-modulus chain))
	 (incf (frame this-start) (this-modulus chain))
	 (setf chain (next-link chain)))))))

;; Volume Changing Functions

(defun db-gain (decibel)
  "Change decibels to a simple multiplier (100db = 1.0)"
  (coerce (expt 10
		(/ (- decibel 100)
		   20))
	  'single-float))

(defun gain (db chain)
  (setf (set-gain chain) (db-gain db))
  (if (next-link chain)
      (gain db (next-link chain)))
  chain)

(defun mute (chain)
  (setf (set-mute chain) t)
  (if (next-link chain)
      (mute (next-link chain)))
  chain)

;; Compiling The Score

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

(defun replace-names-with-links (score)
  (cond
    ((by-name score)
     (make-link (by-name score)))
    ((atom score)
     score)
    (t
     (cons (replace-names-with-links (car score))
	   (replace-names-with-links (cdr score))))))

(defun compile-score ()
  "Return a function that will CYCLE each element of SCORE altogether."
  (let ((funs (mapcar (lambda (track) (play-fun (eval (list 'cycle track))))
		      (replace-names-with-links *score*))))
    (lambda (dst i time start gain)
      (declare (optimize (speed 3) (space 0) (safety 0)
			 (debug 0) (compilation-speed 0)))
      (loop for fun in funs do
	   (funcall (the function fun) dst i time start gain)))))

(defun play-null (&rest args)
  (declare (optimize (speed 3) (space 0) (safety 0)
		     (debug 0) (compilation-speed 0))
	   (ignore args)))
