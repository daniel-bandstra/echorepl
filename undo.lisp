(in-package :echorepl)

(defvar *clip-store* nil)
(defvar *score* nil)

(defvar *clip-store-undo* nil)
(defvar *score-undo* nil)

(defvar *clip-store-redo* nil)
(defvar *score-redo* nil)

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
  (push *clip-store* *clip-store-undo*)
  (push *score* *score-undo*))

(defun undo-pop ()
  (setf *clip-store* (pop *clip-store-undo*)
	*score* (pop *score-undo*))
  (signal-score-update))

(defun redo-push ()
  (push *clip-store* *clip-store-redo*)
  (push *score* *score-redo*))

(defun redo-pop ()
  (setf *clip-store* (pop *clip-store-redo*)
	*score* (pop *score-redo*))
  (signal-score-update))

(declaim (ftype (function ()) play-score))

(defun reset-score ()
  (setf *score* nil)
  (play-score)
  (setf *clip-store* nil
	*clip-store-undo* nil
	*clip-store-redo* nil
	*score-undo* nil
	*score-redo* nil)
  (reset-tick)
  (format t "~&Reset~%")
  (signal-score-update))

(defun undo ()
  (if (and *clip-store-undo* *score-undo*)
      (progn (redo-push)
	     (undo-pop)))
  (play-score))

(defun redo ()
  (if (and *clip-store-redo* *score-redo*)
      (progn (undo-push)
	     (redo-pop)))
  (play-score))
