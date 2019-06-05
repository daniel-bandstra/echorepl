(in-package :echorepl)

(defctype sample-t :float)
(defvar sample-t :float)
(defctype nframes-t :uint32)
(defvar nframes-t :uint32)
(defctype time-t :uint64)
(defvar time-t :uint64)

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

(let ((jack-running nil)
      (client (null-pointer))
      (input (null-pointer))
      (output (null-pointer))
      (so *standard-output*))

  ;; setup and tear-down
  
  (defun connect-outputs ()
    (if jack-running
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
    (if jack-running
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

  (defun start-jack (name callback)
    (if jack-running
	(progn (format so "~&Jack is already running.~%")
	       t)
	(progn (setf client (with-foreign-object (status :int)
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
						0))
	       (if (null-pointer-p client)
		   (progn (format so "~&Could not open Jack.~%")
			  nil)
		   (progn
		     (setf jack-running t)
		     (jack-on-shutdown client
				       (get-callback '*jack-shutdown-callback*)
				       (null-pointer))
		     (jack-set-process-callback client
						(get-callback callback)
						(null-pointer))
		     (jack-activate client)
		     (connect-outputs)
		     (connect-input 0)
		     (setf *sample-rate* (jack-get-sample-rate client))
		     t)))))

  (defun stop-jack ()
    (if jack-running
	(jack-client-close client))
    (setf jack-running nil))

  ;; Functions used in the process callback

  (defun input-buffer (count)
    (jack-port-get-buffer input count))

  (defun output-buffer (count)
    (jack-port-get-buffer output count))

  (defun last-frame-time ()
    (jack-last-frame-time client))

  ;; Time Functions

  (defun usecs ()
    (if jack-running
	(jack-get-time client)
	0))

  (defun usecs-frame (usecs)
    (if jack-running
	(jack-time-to-frames client usecs)
	0))

  (defun frame-usecs (frame)
    (if jack-running
	(jack-frames-to-time client frame)
	0))

  (defun jack-running ()
    jack-running))
