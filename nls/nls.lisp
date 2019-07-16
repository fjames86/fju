
(defpackage #:nls
  (:use #:cl))

(in-package #:nls)

(defconstant +nls-prog+ #x27E1FAEE)
(defconstant +nls-vers+ 1)

(drx:defxstruct share ()
  (hshare :uint64)
  (name :string)
  (version :uint32)
  (seq :uint64)
  (lbacount :uint32)
  (start :uint32)
  (count :uint32)
  (lastid :uint64)
  (flags :uint32))

(drx:defxdecoder prop-res (blk)
  (let ((b (drx:decode-boolean blk)))
    (when b
      (decode-share blk))))

(frpc2:defrpc %call-prop (+nls-prog+ +nls-vers+ 2)
  :uint64
  prop-res)

(defun call-prop (client hshare)
  (%call-prop client hshare))

(drx:defxdecoder list-res (blk)
  (do ((b (drx:decode-boolean blk) (drx:decode-boolean blk))
       (res nil))
      ((not b) (nreverse res))
    (push (decode-share blk) res)))

(frpc2:defrpc call-list (+nls-prog+ +nls-vers+ 1)
  :void
  list-res)

(drx:defxstruct entry ()
  (id :uint64)
  (prev-id :uint64)
  (seq :uint64)
  (flags :uint32)
  (data :opaque))

(drx:defxdecoder read-res (blk)
  (let ((prop (decode-share blk)))
    (do ((res nil)
	 (done nil))
	(done (values (nreverse res) prop))
      (if (drx:decode-boolean blk)
	  (push (decode-entry blk) res)
	  (setf done t)))))

(drx:defxstruct read-arg ((:mode :list))
  (hshare :uint64)
  (id :uint64)
  (count :uint32))

(frpc2:defrpc %call-read (+nls-prog+ +nls-vers+ 3)
  read-arg
  read-res)

(defun call-read (client hshare id &optional xdrcount)
  (%call-read client (list hshare id (or xdrcount (* 32 1024)))))


(drx:defxstruct write-arg ((:mode :list))
  (hshare :uint64)
  (flags :uint32)
  (data :opaque*))
(drx:defxstruct write-res-body ((:mode :list))
  (id :uint64)
  (share share))
(drx:defxdecoder write-res (blk)
  (let ((b (drx:decode-boolean blk)))
    (when b 
      (decode-write-res-body blk))))

(frpc2:defrpc %call-write (+nls-prog+ +nls-vers+ 4)
  write-arg
  write-res)

(defun call-write (client hshare seq &key (start 0) end (flags 0))
  (let ((res (%call-write client (list hshare flags (list seq start (or end (length seq)))))))
    (if res
	(values (car res) (cadr res))
	(values nil nil))))

(drx:defxstruct notify-arg ((:mode :list))
  (hostid :uint64)
  (hshare :uint64)
  (seq :uint64)
  (lastid :uint64))

(frpc2:defrpc %call-notify (+nls-prog+ +nls-vers+ 6)
  notify-arg 
  :void)
(defun call-notify (client hostid hshare seq &optional (lastid 0))
  (%call-notify client (list hostid hshare seq lastid)))

(defmacro with-local-client ((var) &body body)
  `(frpc2:with-rpc-client (,var frpc2:udp-client :addr (fsocket:sockaddr-in #(127 0 0 1) 8000))
     ,@body))

(defmacro with-remote-client ((var addr) &body body)
  `(frpc2:with-rpc-client (,var frpc2:udp-client :addr (fsocket:sockaddr-in ,addr 8000))
     ,@body))

      
