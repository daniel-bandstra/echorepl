(in-package :echorepl)

;; I would have expected MEM-AREF to be the computationally cheapest thing I did all
;; day, but it turns out to be the most expensive, so here we go with this:

(defun sample-read (src offset)
  (declare (type (integer 0 #.most-positive-fixnum) offset)
	   (optimize (speed 3) (space 0) (safety 0) 
		     (debug 0) (compilation-speed 0)))
  (foreign-funcall "sample_read"
		   :pointer src :unsigned-int offset :float))

(defun sample-write (dst offset sample)
  (declare (type (integer 0 #.most-positive-fixnum) offset)
	   (single-float sample)
	   (optimize (speed 3) (space 0) (safety 0)
		     (debug 0) (compilation-speed 0)))
  (foreign-funcall "sample_write"
		   :pointer dst :unsigned-int offset :float sample :void))

;; C memory operations

(defun copy-samples (dst src count)
  (declare (type (integer 0 #.most-positive-fixnum) count)
	   (optimize (speed 3) (space 0) (safety 0)
		     (debug 0) (compilation-speed 0)))
  (foreign-funcall "memcpy"
		   :pointer dst :pointer src
		   :unsigned-int (* count #.(foreign-type-size sample-t))))

(defun zero-samples (dst count)
  (declare (type (integer 0 #.most-positive-fixnum) count)
	   (optimize (speed 3) (space 0) (safety 0)
		     (debug 0) (compilation-speed 0)))
  (foreign-funcall "memset"
		   :pointer dst
		   :int 0
		   :unsigned-int (* count #.(foreign-type-size sample-t))))
