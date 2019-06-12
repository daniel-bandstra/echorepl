(defsystem "echorepl"
  :name "echorepl"
  :version "0.0.1"
  :author "Daniel Bandstra"
  :license "GPLv3"
  :description "echorepl"
  :long-description "A lisp DSL for recording and looping audio"
  :depends-on ("cffi" "bordeaux-threads" "trivial-garbage")
  :serial t
  :components ((:file "package")
	       (:file "callback")
	       (:file "jack")
	       (:file "time")
	       (:file "pedal")
	       (:file "clip")
	       (:file "undo")
	       (:file "play")
	       (:file "record")
	       (:file "file")))
