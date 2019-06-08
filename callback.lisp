(in-package :echorepl)

(defcstruct callback-info
  (monitor :int)

  (client :pointer)

  (in-port :pointer)
  (out-port :pointer)

  (buffer-size nframes-t)

  (in-buf :pointer)
  (frames-buf :pointer)

  (out-buf :pointer))

(defcfun "new_callback_info" :pointer
  (buffer-size :unsigned-int))

(defcfun "delete_callback_info" :void
  (container :pointer))

(defun get-sample-frame (callback-info)
  (declare (optimize (speed 3) (space 0) (safety 0)
		     (debug 0) (compilation-speed 0)))
  (foreign-funcall "get_sample_frame"
		   :pointer callback-info
		   nframes-t))

(defun get-sample (callback-info)
  (declare (optimize (speed 3) (space 0) (safety 0)
		     (debug 0) (compilation-speed 0)))
  (foreign-funcall "get_sample"
		   :pointer callback-info
		   sample-t))
