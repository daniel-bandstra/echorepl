(in-package :echorepl)

;; starting and stopping jack

(defconstant +jack-no-start-server+ #x01)

;; jack client

(defcfun "jack_client_open" :pointer
  (client-name :string)
  (options :int)
  (status :pointer)) ;; pointer to an int

(defcfun "jack_client_close" :int
  (client :pointer))

(defcfun "jack_set_process_callback" :int
  (client :pointer)
  (callback :pointer)
  (arg :pointer))

(let ((so *standard-output*))
  (defcallback *jack-shutdown-callback* :void
      ((arg :pointer))
    (declare (ignore arg))
    (format so "Jack shut down this client.~%")))

(defcfun "jack_on_shutdown" :void
  (client :pointer)
  (callback :pointer)
  (arg :pointer))

;; non-callback api

(defcfun "jack_cycle_wait" nframes-t
  (client :pointer))

(defcfun "jack_cycle_signal" :void
  (client :pointer)
  (status :int)) ;; should be zero

;; ports

(defconstant +jack-default-audio-type+
  (if (boundp '+jack-default-audio-type+)
      (symbol-value '+jack-default-audio-type+)
      "32 bit float mono audio"))
(defconstant +jack-port-is-input+ #x1)
(defconstant +jack-port-is-output+ #x2)
(defconstant +jack-port-is-physical+ #x4)

(defcfun "jack_port_register" :pointer
  (client :pointer)
  (port-name :string)
  (port-type :string)
  (flags :unsigned-long)
  (buffer-size :unsigned-long))

(defcfun "jack_activate" :int
  (client :pointer))

(defcfun "jack_get_ports" :pointer
  (client :pointer)
  (port-name-pattern :string)
  (type-name-pattern :string)
  (flags :unsigned-long))

(defcfun "jack_free" :void
  (ptr :pointer))

(defcfun "jack_port_connected" :int
  (port :pointer))

(defcfun "jack_port_disconnect" :int
  (client :pointer)
  (port :pointer))

(defcfun "jack_connect" :int
  (client :pointer)
  (source-port :string)
  (destination-port :string))

(defcfun "jack_port_name" :string
  (port :pointer))

;; buffers

(defcfun "jack_port_get_buffer" :pointer
  (port :pointer)
  (count nframes-t))

;; timing

(defcfun "jack_get_sample_rate" nframes-t
  (client :pointer))

(defcfun "jack_frame_time" nframes-t
  (client :pointer))

(defcfun "jack_last_frame_time" nframes-t
  (client :pointer))

(defcfun "jack_get_time" time-t
  (client :pointer))

(defcfun "jack_time_to_frames" nframes-t
  (client :pointer)
  (time time-t))

(defcfun "jack_frames_to_time" time-t
  (client :pointer)
  (frames nframes-t))

;; Jack Functions for Lisp

;; The main function here is (start-jack "name" process-fun). PROCESS-FUN is a function
;; that takes the arguments (sample frame out-ptr), i.e. a single sample, that
;; sample's frame-time according to Jack, and a pointer to which a single output sample
;; could be written. Then there's (stop-jack), of course.

(let (
      ;; thread stuff
      (jack-running nil)
      (thread nil)
      (so *standard-output*)
      
      ;; jack pointers
      (callback-info (null-pointer))
      (client (null-pointer))
      (input (null-pointer))
      (output (null-pointer)))

  (defun jack-running ()
    jack-running)
  
  ;; setup and tear-down
  
  (defun connect-outputs ()
    (if (jack-running)
	(let ((ports (jack-get-ports client
				     (null-pointer)
				     (null-pointer)
				     (logior +jack-port-is-physical+
					     +jack-port-is-input+))))
	  (unless (null-pointer-p ports)
	    (if (> (jack-port-connected output) 0)
		(jack-port-disconnect client output))
	    (jack-connect client
			  (jack-port-name output)
			  (mem-ref (mem-aptr ports :pointer 0) :pointer))
	    (jack-connect client
			  (jack-port-name output)
			  (mem-ref (mem-aptr ports :pointer 1) :pointer))
	    (jack-free ports)))))

  (defun connect-input (port-number)
    (if (jack-running)
	(let ((ports (jack-get-ports client
				     (null-pointer)
				     (null-pointer)
				     (logior +jack-port-is-physical+
					     +jack-port-is-output+))))
	  (unless (null-pointer-p ports)
	    (if (> (jack-port-connected input) 0)
		(jack-port-disconnect client input))
	    (jack-connect client
			  (mem-ref (mem-aptr ports :pointer port-number) :pointer)
			  (jack-port-name input))
	    (jack-free ports)))))

  (defun start-jack (name process)
    (if (jack-running)
	(progn (format so "~&Jack is already running.~%")
	       t)
	(progn
	  (setf
	   callback-info (new-callback-info *jack-buffer-size*)
	   client (with-foreign-object (status :int)
		    (jack-client-open name
				      +jack-no-start-server+
				      status))
	   input (jack-port-register client
				     "input"
				     +jack-default-audio-type+
				     +jack-port-is-input+
				     0)
	   output (jack-port-register client
				      "output"
				      +jack-default-audio-type+
				      +jack-port-is-output+
				      0)
	   (foreign-slot-value callback-info '(:struct callback-info) 'client)
	   client
	   (foreign-slot-value callback-info '(:struct callback-info) 'in-port)
	   input
	   (foreign-slot-value callback-info '(:struct callback-info) 'out-port)
	   output)
	  (if (null-pointer-p client)
	      (progn (format so "~&Could not open Jack.~%")
		     nil)
	      (let ((dst (foreign-slot-value callback-info
					     '(:struct callback-info)
					     'out-buf))
		    (divisor (foreign-slot-value callback-info
						 '(:struct callback-info)
						 'buffer-size)))
		(declare (fixnum divisor))
		(setf
		 thread
		 (make-thread
		  (lambda ()
		    (declare (optimize (speed 3) (space 0) (safety 0)
				       (debug 0) (compilation-speed 0)))
		    (loop while jack-running do
			 (let* ((frame (get-sample-frame callback-info))
				(sample (get-sample callback-info)))
			   (declare (fixnum frame))
			   (funcall (the function process)
				    sample
				    frame
				    dst
				    (mod frame divisor)))))))
		(jack-on-shutdown client
				  (get-callback '*jack-shutdown-callback*)
				  (null-pointer))

		(jack-set-process-callback client
					   (foreign-symbol-pointer
					    "echorepl_callback")
					   callback-info)
		(jack-activate client)
		(connect-outputs)
		(connect-input 0)
		(setf *sample-rate* (jack-get-sample-rate client)
		      jack-running t)
		t)))))

  (defun stop-jack ()
    (cond
      (jack-running (setf jack-running nil)
		    (handler-case
			(if thread (join-thread thread))
		      (t (c)
			(format so "~&Caught error:~%~a~%" c)))
		    (setf thread nil)
		    (jack-client-close client)
		    (delete-callback-info callback-info))))

  ;; Time Functions

  (defun usecs ()
    (if (jack-running)
	(jack-get-time client)
	0))

  (defun usecs-frame (usecs)
    (if (jack-running)
	(jack-time-to-frames client usecs)
	0))

  (defun frame-usecs (frame)
    (if (jack-running)
	(jack-frames-to-time client frame)
	0))

  ;; software monitoring
  (defun monitor ()
    (if (jack-running)
	(setf
	 (foreign-slot-value callback-info '(:struct callback-info) 'monitor)
	 (if
	  (zerop (foreign-slot-value callback-info '(:struct callback-info)
				     'monitor))
	  1
	  0)))))
