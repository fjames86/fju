
(in-package #:fvm)

(defmacro with-rpc-client ((var) &body body)
  `(frpc2:with-rpc-client (,var frpc2:udp-client :addr (fsocket:sockaddr-in #(127 0 0 1) 8000))
     ,@body))

(drx:defxstruct run-args ((:mode :list))
  (program :opaque)
  (timeout :uint32))

(drx:defxstruct load-args ((:mode :list))
  (progdata :opaque)
  (flags :uint32)
  (inlog-id :uint64)
  (outlog-id :uint64)
  (name :string))
(frpc2:defrpc %call-load (+fvm-prog+ 1 1) load-args :uint32)
(defun call-load (progdata &key (start t) autounload-p inlog-id outlog-id name)
  (with-rpc-client (c)
    (%call-load c (list progdata
			start
			(let ((flags 0))
			  (when autounload-p (setf flags (logior flags #x1)))
			  (when start (setf flags (logior flags #x2)))
			  flags)
			(or inlog-id 0)
			(or outlog-id 0)
			(or name "")))))

(frpc2:defrpc %call-unload (+fvm-prog+ 1 2) :uint32 :void)
(defun call-unload (id)
  (with-rpc-client (c)
    (%call-unload c id)))


(drx:defxstruct list-res-body ((:mode :plist))
  (id :uint32)
  (flags :uint32)
  (tickcount :uint64)
  (runtime :uint64)
  (inlog-id :uint64)
  (outlog-id :uint64)
  (name :string))
(drx:defxlist list-res* () list-res-body)
(drx:defxoptional list-res () list-res*)
(frpc2:defrpc %call-list (+fvm-prog+ 1 3) :void list-res)
(defun call-list ()
  (with-rpc-client (c)
    (%call-list c)))

(drx:defxstruct pause-args ((:mode :list))
  (id :uint32)
  (stop :uint32))
(frpc2:defrpc %call-pause (+fvm-prog+ 1 4) pause-args :void)
(defun call-pause (id cmd)
  (with-rpc-client (c)
    (%call-pause c
		 (list id (ecase cmd
			    (:continue 0)
			    (:stop 1)
			    (:reset 2))))))


(defconstant +freg-prog+ #x27E1FB10)

(drx:defxstruct freg-put-args ((:mode :list))
  (parentid :uint64)
  (name :string)
  (flags :uint32)
  (data :opaque))
(frpc2:defrpc %call-freg-put (+freg-prog+ 1 3) freg-put-args :boolean)
(defun call-freg-put (parentid name type data)
  (let ((flags (ecase type
		 (:opaque 0)
		 (:uint32 1)
		 (:uint64 2)
		 (:string 3)
		 (:key 4)))
	(data (ecase type
		(:opaque data)
		(:uint32 (let ((buf (make-array 4 :element-type '(unsigned-byte 8))))
			(setf (nibbles:ub32ref/le buf 0) data)
			buf))
		(:uint64 (let ((buf (make-array 8 :element-type '(unsigned-byte 8))))
			(setf (nibbles:ub64ref/le buf 0) data)
			buf))
		(:string (babel:string-to-octets data))
		(:key (make-array 0 :element-type '(unsigned-byte 8))))))
    (with-rpc-client (c)
      (%call-freg-put c (list parentid name flags data)))))

(drx:defxstruct freg-get-args ((:mode :list))
  (path :string)
  (flags :uint32))
(drx:defxdecoder freg-get-res (blk)
  (let ((sts (drx:decode-boolean blk)))
    (when sts
      (let ((id (drx:decode-uint64 blk))
	    (flags (drx:decode-uint32 blk)))
	(values id 
		(case (logand flags #x0f)
		  (0 (drx:decode-opaque blk))
		  (1 (drx:decode-uint32 blk))
		  (2 (drx:decode-uint64 blk))
		  (3 (drx:decode-string blk))
		  (4 (let ((n (drx:decode-uint32 blk)))
		       (loop :for i :below n :collect (drx:decode-uint64 blk))))))))))
      
(frpc2:defrpc %call-freg-get (+freg-prog+ 1 5) freg-get-args freg-get-res)
(defun call-freg-get (path type)
  (with-rpc-client (c)
    (%call-freg-get c (list path (ecase type
				   (:opaque 0)
				   (:uint32 1)
				   (:uint64 2)
				   (:string 3)
				   (:key 4))))))


(defun install-event-program (name category eventid)
  (let ((parentid (call-freg-get "/fju/fvm/event" :key)))
    (call-freg-put parentid name :key nil)
    (let ((parentid (call-freg-get (format nil "/fju/fvm/event/~A" name) :key)))
      (call-freg-put parentid "category" :uint32 category)
      (call-freg-put parentid "eventid" :uint32 eventid))))

(defun install-startup-program (name)
  (let ((parentid (call-freg-get "/fju/fvm/startup" :key)))
    (call-freg-put parentid name :string name)))

(defun install-program (name progdata &key startp autounloadp inlogid outlogid)
  (let ((parentid (call-freg-get "/fju/fvm/programs" :key)))
    (call-freg-put parentid name :key nil)
    (let ((parentid (call-freg-get (format nil "/fju/fvm/programs/~A" name) :key))
	  (flags 0))
      (when autounloadp (setf flags (logior flags #x0001)))
      (when startp (setf flags (logior flags #x0002)))
      
      (call-freg-put parentid "progdata" :opaque progdata)
      (call-freg-put parentid "flags" :uint32 flags)
      (when inlogid
	(call-freg-put parentid "inlogid" :uint64 inlogid))
      (when outlogid
	(call-freg-put parentid "outlogid" :uint64 outlogid)))))






  
