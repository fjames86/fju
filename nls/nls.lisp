
(defpackage #:nls
  (:use #:cl))

(in-package #:nls)

(defconstant +nls-prog+ #x27E1FAEE)
(defconstant +nls-vers+ 1)

(drx:defxstruct share ((:mode :plist))
  (hshare :uint64)
  (name :string))

(drx:defxstruct prop ()
  (version :uint32)
  (seq :uint64)
  (lbacount :uint32)
  (start :uint32)
  (count :uint32)
  (last-id :uint64)
  (flags :uint32)
  (tag :uint64))

(drx:defxstruct share-prop ((:mode :list))
  (share share)
  (prop prop))

(drx:defxdecoder prop-res (blk)
  (let ((b (drx:decode-boolean blk)))
    (when b
      (decode-share-prop blk))))

(frpc2:defrpc %call-prop (+nls-prog+ +nls-vers+ 2)
  :uint64
  prop-res)

(defun call-prop (client hshare)
  (let ((res (%call-prop client hshare)))
    (when res
      (destructuring-bind (share prop) res 
	(values prop share)))))

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
  (do ((res nil)
       (done nil))
      (done (nreverse res))
    (if (drx:decode-boolean blk)
	(push (decode-entry blk) res)
	(setf done t))))

(drx:defxstruct read-arg ((:mode :list))
  (hshare :uint64)
  (id :uint64)
  (count :uint32))

(frpc2:defrpc %call-read (+nls-prog+ +nls-vers+ 3)
  read-arg
  read-res)

(defun call-read (client hshare id &optional (count 1))
  (%call-read client (list hshare id count)))


(drx:defxstruct write-arg ((:mode :list))
  (hshare :uint64)
  (data :opaque*))
(drx:defxstruct write-res ((:mode :list))
  (successp :boolean)
  (id :uint64))

(frpc2:defrpc %call-write (+nls-prog+ +nls-vers+ 4)
  write-arg
  write-res)

(defun call-write (client hshare seq &key (start 0) end)
  (let ((res (%call-write client (list hshare (list seq start (or end (length seq)))))))
    (when (car res)
      (cadr res))))

(defmacro with-local-client ((var) &body body)
  `(frpc2:with-rpc-client (,var frpc2:udp-client :addr (fsocket:sockaddr-in #(127 0 0 1) 8000))
     ,@body))

      
