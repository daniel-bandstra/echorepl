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
	       (:file "jack")
	       (:file "time")
	       (:file "pedal")
	       (:file "sample")
	       (:file "clip")
	       (:file "play")
	       (:file "record")
	       (:file "file")))
