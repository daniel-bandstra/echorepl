(in-package :echorepl)

(defvar *clip-store* nil)
(defvar *score* nil)

(defvar *undo-clip-store* nil)
(defvar *undo-score* nil)

(defvar *redo-clip-store* nil)
(defvar *redo-score* nil)

(let ((emacs-connection (if (find-package 'swank)
			    swank::*emacs-connection*)))
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
	    (null))))))

(defun undo-push ()
  (push *clip-store* *undo-clip-store*)
  (push *score* *undo-score*))

(defun undo-pop ()
  (setf *clip-store* (pop *undo-clip-store*)
	*score* (pop *undo-score*))
  (signal-score-update))

(defun redo-push ()
  (push *clip-store* *redo-clip-store*)
  (push *score* *redo-score*))

(defun redo-pop ()
  (setf *clip-store* (pop *redo-clip-store*)
	*score* (pop *redo-score*))
  (signal-score-update))

(declaim (ftype (function ()) play-score))

(defun reset-score ()
  (setf *score* nil)
  (play-score)
  (setf *clip-store* nil
	*undo-clip-store* nil
	*redo-clip-store* nil
	*undo-score* nil
	*redo-score* nil)
  (reset-tick)
  (format t "~&Reset~%")
  (signal-score-update))

(defun undo ()
  (if (and *undo-clip-store* *undo-score*)
      (progn (redo-push)
	     (undo-pop)))
  (play-score))

(defun redo ()
  (if (and *redo-clip-store* *redo-score*)
      (progn (undo-push)
	     (redo-pop)))
  (play-score))
