
(in-package #:fvm)

(defmacro with-rpc-client ((var) &body body)
  `(frpc2:with-rpc-client (,var frpc2:udp-client :addr (fsocket:sockaddr-in #(127 0 0 1) 8000))
     ,@body))

(drx:defxstruct run-args ((:mode :list))
  (program :opaque)
  (timeout :uint32))

(defconstant +fvm-prog+ #x27E1FB11)

(drx:defxstruct load-args ((:mode :list))
  (progdata :opaque)
  (start :boolean)
  (flags :uint32)
  (nls-id :uint64))
(frpc2:defrpc %call-load (+fvm-prog+ 1 1) load-args :uint32)
(defun call-load (progdata &key (start t) autounload-p nls-id)
  (with-rpc-client (c)
    (%call-load c (list progdata
			start
			(let ((flags 0))
			  (when autounload-p (setf flags (logior flags #x1)))
			  (when nls-id (setf flags (logior flags #x2)))
			  flags)
			(or nls-id 0)))))

(frpc2:defrpc %call-unload (+fvm-prog+ 1 2) :uint32 :void)
(defun call-unload (id)
  (with-rpc-client (c)
    (%call-unload c id)))


(drx:defxstruct list-res-body ((:mode :plist))
  (id :uint32)
  (flags :uint32)
  (nls-id :uint64))
(drx:defxlist list-res* () list-res-body)
(drx:defxoptional list-res () list-res*)
(frpc2:defrpc %call-list (+fvm-prog+ 1 3) :void list-res)
(defun call-list ()
  (with-rpc-client (c)
    (%call-list c)))

(drx:defxstruct pause-args ((:mode :list))
  (id :uint32)
  (stop :boolean))
(frpc2:defrpc %call-pause (+fvm-prog+ 1 4) pause-args :void)
(defun call-pause (id &optional (stop t))
  (with-rpc-client (c)
    (%call-pause c (list id stop))))


