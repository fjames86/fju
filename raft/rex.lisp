
(defpackage #:rex
  (:use #:cl)
  (:export #:call-read
	   #:call-write))
	   

(in-package #:rex)

(defconstant +rex-prog+ #x27E1FAF0)
(defconstant +rex-vers+ 1)

(frpc2:defrpc %call-read (+rex-prog+ +rex-vers+ 1)
  :uint64
  :opaque)

(defmacro with-client ((var &optional addr) &body body)
  `(frpc2:with-rpc-client (,var frpc2:udp-client :addr (fsocket:sockaddr-in ,(or addr #(127 0 0 1)) 8000))
     ,@body))

(defun call-read (addr clid)
  (with-client (c addr)
    (%call-read c clid)))

(drx:defxstruct write-args ((:mode :list))
  (clid :uint64)
  (buf :opaque))

(drx:defxstruct write-res ((:mode :list))
  (success :boolean)
  (leaderid :uint64))

(frpc2:defrpc %call-write (+rex-prog+ +rex-vers+ 2)
  write-args
  write-res)

(defun call-write (addr clid buf)
  (with-client (c addr)
    (apply #'values (%call-write c (list clid (coerce buf '(vector (unsigned-byte 8))))))))





