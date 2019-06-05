(defpackage :echorepl
  (:use :common-lisp :cffi :bordeaux-threads :trivial-garbage))

(in-package :echorepl)

(export
 '(
   start-recording
   stop-recording
   reset-score
   loop-button
   undo-button
   play-score
   time-rate
   reverse-time
   connect-input
   pedal-open
   save-project
   load-project
   ))

(define-foreign-library engine
  (t (:default #.(namestring
		(asdf:system-relative-pathname 'echorepl "engine/libengine")))))

(use-foreign-library engine)

(define-foreign-library sndfile
  (t (:default "/usr/lib/x86_64-linux-gnu/libsndfile")))

(use-foreign-library sndfile)
