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

;;; variables to edit for your setup

(defparameter *sample-rate* 44100) ;; this also gets set when Jack starts up

(defparameter *latency* 6805)
;; This variable is a number of whole samples or frames. You can measure this
;; with the jack_delay utility and a patch cable.

;;; foreign library definitions

;; homegrown packages

(define-foreign-library engine
  (t (:default #.(namestring
		(asdf:system-relative-pathname 'echorepl "engine/libengine")))))

(use-foreign-library engine)

(define-foreign-library pedal
  (t (:default #.(namestring
		  (asdf:system-relative-pathname 'echorepl "pedal/libpedal")))))

(use-foreign-library pedal)

;; library dependencies

(define-foreign-library sndfile
  (t (:default "/usr/lib/x86_64-linux-gnu/libsndfile")))

(use-foreign-library sndfile)

(define-foreign-library jack
  (t (:default "libjack")))

(use-foreign-library jack)
