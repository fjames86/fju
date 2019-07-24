
(defpackage #:raft
  (:use #:cl))

(in-package #:raft)

(defconstant +raft-prog+ #x27E1FAEF)
(defconstant +raft-vers+ 1)

(drx:defxstruct rmember ()
  (hostid :uint64)
  (lastseen :uint64)
  (flags :uint32)
  (nextseq :uint64)
  (stateterm :uint64))
  

(drx:defxarray member-list ((:mode :list)) rmember)

(drx:defxstruct cluster ()
  (clid :uint64)
  (leader :uint64)
  (termseq :uint64)
  (voteid :uint64)
  (state :uint32)
  (typeid :uint32)
  (commitseq :uint64)
  (stateseq :uint64)
  (stateterm :uint64)
  (members member-list))

(drx:defxarray cluster-list ((:mode :list)) cluster)

(frpc2:defrpc %call-list (+raft-prog+ +raft-vers+ 3)
  :void
  cluster-list)

(defun call-list (addr port)
  (ignore-errors 
    (frpc2:with-rpc-client (c frpc2:udp-client
			      :addr (fsocket:sockaddr-in addr port)
			      :retry 1
			      :timeout 250)
      (%call-list c))))

(defun poll-cluster (addrs port)
  (do ((done nil))
      (done)
    (sleep 1)
    (let ((nl nil))
      (dolist (addr addrs)
	(let ((cls (call-list addr port)))	
	  (dolist (cl cls)
	    (unless nl
	      (multiple-value-bind (s m h d mth y) (decode-universal-time (get-universal-time))
		(format t "~A-~A-~A ~A:~A:~A~%" y mth d h m s)))
	    (setf nl t)
	    (format t "~A CLUSTER ~X LEADER=~X TERMSEQ=~A STATE=~A STATESEQ=~A STATETERM=~A~%"
		    (fsocket:sockaddr-string (fsocket:sockaddr-in addr port))
		    (cluster-clid cl) (cluster-leader cl) (cluster-termseq cl)
		    (case (cluster-state cl)
		      (0 "FOLLOWER")
		      (1 "CANDIDATE")
		      (2 "LEADER"))
		    (cluster-stateseq cl) (cluster-stateterm cl)))))
      (when nl (terpri)))))



  
