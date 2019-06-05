(in-package :echorepl)

;; For when my kids want to press the pedal buttons.
;; READ-PEDAL gets the time from Jack, so if you haven't connected Lisp to Jack at
;; least once, there will be an unhandled memory fault.

(setf *colors-running* nil)
(progn
  (defparameter *colors-running* t)
  (let ((a-state 0)
	(a-colors '(0 0 0))
	(b-state 2)
	(b-colors '(0 0 0)))
    (loop while *colors-running* do
	 (progn
	   (case (pedal-read)
	     (:a-down
	      (setf a-colors
		    (case a-state
		      (0 '(12 0 0))
		      (1 '(0 8 0))
		      (2 '(0 0 8))
		      (3 '(0 0 0)))
		    a-state (mod (1+ a-state) 4)))
	     (:b-down
	      (setf b-colors
		    (case b-state
		      (0 '(12 0 0))
		      (1 '(0 8 0))
		      (2 '(0 0 8))
		      (3 '(0 0 0)))
		    b-state (mod (1- b-state) 4))))
	   (pedal-color
		      (+ (car a-colors) (car b-colors))
		      (+ (cadr a-colors) (cadr b-colors))
		      (+ (caddr a-colors) (caddr b-colors)))))))
