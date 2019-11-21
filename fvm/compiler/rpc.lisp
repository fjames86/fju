
(in-package #:fvm)

(defmacro with-rpc-client ((var) &body body)
  `(frpc2:with-rpc-client (,var frpc2:udp-client :addr (fsocket:sockaddr-in #(127 0 0 1) 8000))
     ,@body))

(drx:defxstruct run-args ((:mode :list))
  (program :opaque)
  (timeout :uint32))

(defconstant +fvm-prog+ #x27E1FB11)

(frpc2:defrpc %call-start (+fvm-prog+ 1 1) :opaque :uint32)

(defun call-start (program)
  (with-rpc-client (c)
    (%call-start c program)))

(frpc2:defrpc %call-stop (+fvm-prog+ 1 2) :uint32 :void)
(defun call-stop (id)
  (with-rpc-client (c)
    (%call-stop c id)))


(drx:defxlist list-res () :uint32)
(frpc2:defrpc %call-list (+fvm-prog+ 1 3) :void list-res)
(defun call-list ()
  (with-rpc-client (c)
    (%call-list c)))




