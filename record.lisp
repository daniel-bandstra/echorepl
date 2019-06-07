(in-package :echorepl)

;; thread-do, to put resource-intensive things on a back burner

(defmacro thread-do (&rest body)
  (let ((result (gensym))
	(thread (gensym)))
    `(let* ((,result nil)
	    (,thread
	     (make-thread (lambda ()
			    (setf ,result
				  (progn ,@body))))))
       (join-thread ,thread)
       ,result)))

;; main record/loop/play farrago

(let (
      ;; state variables
      (running nil)
      (state 0)

      ;; playback, timing, and clips
      (play-fun #'play-null)
      (master-gain 1.0)
      (input-clip (empty-clip)) ;; place-holder
      (output-start (number-moment 0))
      (clip-start (number-moment 0))
      (parent-modulus 0)
      (process nil)

      ;; for signal-score-update
      (emacs-connection (if (find-package 'swank)
			    swank::*emacs-connection*)))
  
  (declare (boolean running)
	   (fixnum state)
	   (single-float master-gain)
	   (clip input-clip)
	   (moment output-start clip-start))
  
  (defun signal-score-update ()
    (if emacs-connection
	(let ((event `(:ed-rpc-no-wait
		       ,(swank::symbol-name-for-emacs 'echorepl-update-score)
		       nil)))
	  (etypecase emacs-connection
	    (swank::multithreaded-connection
	     (swank::send (swank::mconn.control-thread emacs-connection) event))
	    (swank::singlethreaded-connection
	     (swank::dispatch-event emacs-connection event))
	    (null)))))
  
  ;; recording samples, with interpolation
  (let ((prev-sample 0.0)
	(prev-moment (number-moment 0)))
    (declare (single-float prev-sample)
	     (moment prev-moment))

    (defun record-sample (sample moment)
      (declare (single-float sample)
	       (moment moment)
	       (optimize (speed 3) (space 0) (safety 0)
			 (debug 1) (compilation-speed 0)))
      (flet ((r-s (sample-a moment-a sample-b moment-b)
	       (declare (single-float sample-a sample-b)
			(moment moment-a moment-b))
	       (let* ((moment-diff (moment- moment-b moment-a))
		      (t-diff (+ (frame moment-diff) (fraction moment-diff)))
		      (s-diff (- sample-b sample-a))
		      (tape-len (tape-len input-clip)))
		 (declare (type (integer 0 #.most-positive-fixnum) tape-len))
		 (loop
		    for i fixnum from (1+ (frame moment-a)) upto (frame moment-b)
		    for n single-float from (- 1.0 (fraction moment-a)) by 1.0 do
		      (setf (mem-ref (samples input-clip)
				     'sample-t
				     (the fixnum (* (mod i tape-len)
						    #.(foreign-type-size 'sample-t))))
			    (+ sample-a (* s-diff
					   (/ n t-diff))))))))
	(declare (inline r-s))
	(if (moment< prev-moment moment)
	    (r-s prev-sample prev-moment sample moment)
	    (r-s sample moment prev-sample prev-moment)))
      (setf prev-sample sample
	    prev-moment moment)))

  ;; playing the score

  (defun play-score ()
    (setf play-fun (thread-do
		    (compile-score *score* *clip-store*)))
    (if *score*
	(pedal-color 0 8 0)
	(progn (pedal-color 0 0 0)
	       (setf parent-modulus 0)))
    (signal-score-update))
  
  (defun set-play-fun (&optional fun)
    (if fun
	(setf play-fun fun)
	(play-score)))
  
  ;; Looper Control Functions
  (defun loop-button (&optional (time (now-moment)))
    (case state
      (0 (setf clip-start time)
	 (format t "~&Record~%")
	 (pedal-color 12 0 0))
      (1 (let ((new-clip
		(thread-do (create-clip input-clip
					clip-start
					time
					parent-modulus))))
	   (if (zerop (modulus new-clip)) ;; probably an accidental button-tap
	       (decf state) ;; so keep recording
	       (progn
		 (if (zerop parent-modulus)
		     (setf output-start (if (moment< time clip-start)
					    time clip-start)
			   parent-modulus (modulus new-clip))
		     (setf (offset new-clip)
			   (let ((big-offset (moment- (if (moment< time clip-start)
							  time
							  clip-start)
						      output-start)))
			     (multiple-value-bind (discard frame)
				 (round (frame big-offset) (modulus new-clip))
			       (declare (ignore discard))
			       (moment frame
				       (fraction big-offset))))))
		 (push new-clip *clip-store*)
		 (push (name new-clip)
		       *score*)
		 (play-score)
		 (format t "~&Play ~a~%" (name new-clip)))))))
    (setf state (mod (1+ state) 2)))
  
  (defun undo-button ()
    (if (and (zerop state)
	     (pop *score*))
	(pop *clip-store*))
    (play-score)
    (setf state 0)
    (format t "~&Undo~%")
    (pedal-blink 12 8 0))
  
  (defun master-reverse ()
    (reverse-time)
    (format t "~&Reverse~%"))
  
  (setf process
	(lambda (sample frame out)
	  (declare (optimize (speed 3) (space 0) (safety 0)
			     (debug 0) (compilation-speed 0)))
	  (let ((play-now (tick frame))
		(rec-now (frame-moment frame)))
	    (record-sample sample rec-now)
	    (funcall (the function play-fun)
		     out play-now output-start master-gain))))
  
  (defun start-recording (&optional (buffer-length 600))
    (if running
	(format t "~&Recorder is already running.~%")
	(make-thread
	 (lambda ()
	   ;; recording setup
	   (reset-tick)
	   (play-score)
	   (if (start-jack "echorepl" process)
	       (progn
		 (setf running t
		       input-clip
		       (let ((new-clip (make-clip :clip-len (* buffer-length
							       *sample-rate*))))
			 (clip-setup new-clip)
			 (setf (modulus new-clip) (tape-len new-clip)
			       (clip-start-pos new-clip) 0
			       (fadein-start-pos new-clip) 0
			       (fadein-end-pos new-clip) 0
			       (fadeout-start-pos new-clip) (tape-len new-clip)
			       (fadeout-end-pos new-clip) (tape-len new-clip))
			 new-clip))
		 (set-edge-space-defaults)
		 (sleep (/ (+ *default-half-splice* *default-fudge-factor*)
			   *sample-rate*))
		 (unless *score* (pedal-blink 0 8 0))
		 (format t "~&Ok, ready to record!~%")

		 ;; pedal event loop
		 (handler-case
		     (loop while running do
			  (multiple-value-bind (event moment)
			      (pedal-read)
			    (case event
			      (:a-down (loop-button moment))
			      (:b-long-tap (undo-button))
			      (:b-double-tap (master-reverse)))))
		   (t (c)
		     (format t "~&Caught error:~%~a" c)))
		 
		 ;; cleanup
		 (stop-jack)
		 (pedal-color 0 0 0)
		 (pedal-blink 12 0 0)
		 (format t "~&All done!~%")
		 (sleep 0.1)
		 (setf state 0
		       master-gain 1.0
		       input-clip (empty-clip)
		       output-start (number-moment 0)
		       clip-start (number-moment 0))
		 (gc :full t)))))))

  (defun stop-recording ()
    (setf running nil)))

(defun reset-score ()
  (setf *clip-store* nil
	*score* nil)
  (reset-tick)
  (play-score)
  (format t "~&Reset~%"))
