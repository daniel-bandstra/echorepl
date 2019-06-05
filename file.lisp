(in-package :echorepl)

(defctype sf-count-t :int64)

(defconstant +sf-read+ #x10)
(defconstant +sf-write+ #x20)
(defconstant +sf-format-wav+ #x010000)
(defconstant +sf-format-float+ #x0006)
(defconstant +sf-file-format+ (logior +sf-format-wav+
				      +sf-format-float+))

(defcstruct sf-info
  (frames sf-count-t)
  (sample-rate :int)
  (channels :int)
  (format :int)
  (sections :int)
  (seekable :int))

;; file open/close

(defcfun "sf_open" :pointer
  (path :string)
  (mode :int)
  (info :pointer))

(defcfun "sf_write_sync" :void
  (file :pointer))

(defcfun "sf_close" :int
  (file :pointer))

;; read/write samples

(defcfun "sf_read_float" sf-count-t
  (file :pointer)
  (dst :pointer)
  (count sf-count-t))

(defcfun "sf_write_float" sf-count-t
  (file :pointer)
  (src :pointer)
  (count sf-count-t))

;; string data

(defconstant +sf-str-comment+ #x05)

(defcfun "sf_get_string" :string
  (file :pointer)
  (str-type :int))

(defcfun "sf_set_string" :int
  (file :pointer)
  (str-type :int)
  (str :string))

;; save a clip

(defun clip-string (clip)
  (prin1-to-string `(let ((new-clip
			   (make-clip
			    :name ,(name clip)
			    :sample-rate ,(sample-rate clip)
			    :clip-len ,(clip-len clip)
			    :half-splice ,(half-splice clip)
			    :fudge-factor ,(fudge-factor clip)
			    :tape (create-tape ,(tape-len clip))
			    :parent-modulus ,(parent-modulus clip)
			    :modulus ,(modulus clip)
			    :offset (moment ,(frame (offset clip))
					    ,(fraction (offset clip))))))
		      (setf
		       (clip-start-pos new-clip) ,(clip-start-pos clip)
		       (fadein-start-pos new-clip) ,(fadein-start-pos clip)
		       (fadein-end-pos new-clip) ,(fadein-end-pos clip)
		       (fadeout-start-pos new-clip) ,(fadeout-start-pos clip)
		       (fadeout-end-pos new-clip) ,(fadeout-end-pos clip)
		       (splice new-clip) ,(splice clip))
		      new-clip)))

(defmethod save-clip ((clip clip) (path string))
  (with-foreign-objects ((info '(:struct sf-info)))
    (setf
     (foreign-slot-value info '(:struct sf-info) 'sample-rate) (sample-rate clip)
     (foreign-slot-value info '(:struct sf-info) 'channels) 1
     (foreign-slot-value info '(:struct sf-info) 'format) +sf-file-format+)
    (let ((file (sf-open path
			 +sf-write+
			 info)))
      (sf-write-float file (samples clip) (tape-len clip))
      (sf-set-string file
		     +sf-str-comment+
		     (clip-string clip))
      (sf-write-sync file)
      (sf-close file))))

(defmethod load-clip ((path string))
  "Read a file created with SAVE-LOOP"
  (with-foreign-object (info '(:struct sf-info))
    (setf
     (foreign-slot-value info '(:struct sf-info) 'sample-rate) *sample-rate*
     (foreign-slot-value info '(:struct sf-info) 'channels) 1
     (foreign-slot-value info '(:struct sf-info) 'format) 0)
    (let ((file (sf-open path
			 +sf-read+
			 info))
	  (clip nil))
      (if (= (foreign-slot-value info '(:struct sf-info) 'format)
	     +sf-file-format+)
	  (let ((string (sf-get-string file +sf-str-comment+)))
	    (setf clip (eval (read-from-string string)))
	    (sf-read-float file (samples clip) (tape-len clip)))
	  (format t "File ~a was in the wrong format.~%" path))
      (sf-close file)
      clip)))

;; pathname stuff

(defun pathname-to-directory (pathname)
  "Make sure PATHNAME is a thing that ends with a directory"
  (let ((hanging-name (pathname-name pathname)))
    (if hanging-name
	(merge-pathnames (make-pathname :directory `(:relative ,hanging-name))
			 (make-pathname :directory (pathname-directory pathname)))
	(make-pathname :directory (pathname-directory pathname)))))

(defun save-project (directory &optional (score *score*) (clips *clip-store*))
  (let ((directory (pathname-to-directory directory)))
    (ensure-directories-exist directory)
    (map nil
	 (lambda (name)
	   (save-clip (by-name name clips)
		      (concatenate 'string
				   (directory-namestring (truename directory))
				   (symbol-name name)
				   ".WAV")))
	 (clip-names-in-score score clips))
    (let ((score-path (concatenate 'string
				   (directory-namestring (truename directory))
				   "score.lisp")))
      (with-open-file (s (merge-pathnames score-path) :direction :output
			 :if-exists :overwrite
			 :if-does-not-exist :create)
	(prin1 score s)))))

(defmacro load-project (directory
			&optional (score '*score*) (clips '*clip-store*))
  `(let ((directory (probe-file (pathname-to-directory ,directory))))
     (if directory
	 (progn
	   (setf ,clips
		 (map 'list
		      (lambda (file) (load-clip (namestring file)))
		      (directory (merge-pathnames (make-pathname :name :wild :type "WAV")
						  directory)))
		 ,score
		 (let ((score-path (concatenate 'string
						(directory-namestring (truename directory))
						"score.lisp")))
		   (if (probe-file score-path)
		       (with-open-file (s (merge-pathnames score-path)
					  :direction :input)
			 (read s))
		       (mapcar #'name ,clips))))))))
