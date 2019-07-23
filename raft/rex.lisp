
(defpackage #:rex
  (:use #:cl))

(in-package #:rex)

(defconstant +rex-prog+ #x27E1FAF0)
(defconstant +rex-vers+ 1)

(frpc2:defrpc %call-read (+rex-prog+ +rex-vers+ 1)
  :uint64
  :opaque)

(defun call-read (c clid)
  (%call-read c clid))

(drx:defxstruct write-args ((:mode :list))
  (clid :uint64)
  (buf :opaque))

(drx:defxstruct write-res ((:mode :list))
  (success :boolean)
  (leaderid :uint64))

(frpc2:defrpc %call-write (+rex-prog+ +rex-vers+ 2)
  write-args
  write-res)

(defun call-write (c clid buf)
  (apply #'values (%call-write c (list clid (coerce buf '(vector (unsigned-byte 8)))))))

(defmacro with-client ((var &optional addr) &body body)
  `(frpc2:with-rpc-client (,var frpc2:udp-client :addr (fsocket:sockaddr-in ,(or addr #(127 0 0 1)) 8000))
     ,@body))



