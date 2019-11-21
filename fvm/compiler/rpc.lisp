
(in-package #:fvm)

(defmacro with-rpc-client ((var) &body body)
  `(frpc2:with-rpc-client (,var frpc2:udp-client :addr (fsocket:sockaddr-in #(127 0 0 1) 8000))
     ,@body))

(drx:defxstruct run-args ((:mode :list))
  (program :opaque)
  (timeout :uint32))

(defconstant +fvm-prog+ #x27E1FB11)
(frpc2:defrpc %call-run (+fvm-prog+ 1 1) run-args :void)

(defun call-run (program &optional timeout)
  (with-rpc-client (c)
    (%call-run c (list program (or timeout 0)))))

(frpc2:defrpc %call-start (+fvm-prog+ 1 2) :opaque :void)

(defun call-start (program)
  (with-rpc-client (c)
    (%call-start c program)))




