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
   undo
   redo
   play-score
   update-score
   time-rate
   reverse-time
   connect-input
   monitor
   pedal-open
   save-project
   load-project
   ))

;;; variables to edit for your setup

(defparameter *sample-rate* 44100) ;; this also gets set when Jack starts up

(defparameter *latency* 472)
;; *LATENCY* is a number of whole samples or frames. You can measure this
;; with the jack_delay utility and a patch cable.
(defparameter *jack-buffer-size* #.(expt 2 15))

(defparameter *pedal-port* "/dev/ttyUSB0")

;; Jack types for cffi
(defctype sample-t :float)
(defvar sample-t :float)
(defctype nframes-t :uint32)
(defvar nframes-t :uint32)
(defctype time-t :uint64)
(defvar time-t :uint64)

;;; foreign library definitions

;; library dependencies

(define-foreign-library sndfile
  (t (:default "libsndfile")))

(use-foreign-library sndfile)

(define-foreign-library jack
  (t (:default "libjack")))

(use-foreign-library jack)

;; homegrown packages

(define-foreign-library engine
  (t (:default #.(namestring
		(asdf:system-relative-pathname 'echorepl "engine/libengine")))))

(use-foreign-library engine)

(define-foreign-library pedal
  (t (:default #.(namestring
		  (asdf:system-relative-pathname 'echorepl "pedal/libpedal")))))

(use-foreign-library pedal)
