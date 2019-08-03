
(defpackage #:hrauth
  (:use #:cl))

(in-package #:hrauth)

(defconstant +hrauth+ #x27e1daf0)

(drx:defxfixed key () 32)

(drx:defxstruct cred ((:mode :plist))
  (nonce :uint32)
  (skey key)
  (service :uint32)
  (window :uint32)
  (cipher :uint32))

(drx:defxfixed ecred () 64)

(drx:defxstruct fullname ((:mode :plist))
  (id :uint64)
  (cipher :uint32)
  (ecred ecred))

(defconstant +nickname+ 0)
(defconstant +full+ 1)

(drx:defxunion auth ()
  (+nickname+ :uint32)
  (+full+ fullname))

(drx:defxstruct verf ((:mode :plist))
  (nonce :uint32)
  (timestamp :uint32)
  (tverf :uint32)
  (nickname :uint32))

(defclass hrauth-provider (frpc2:client-provider frpc2:server-provider)
  ((remoteid :initarg :remoteid :reader hrauth-remoteid)
   (localid :initarg :localid :Reader hrauth-localid)
   (key :initarg :key :reader hrauth-key)
   (skey :initarg :skey :reader hrauth-skey)
   (service :initarg :service :initform 0 :reader hrauth-service)
   (window :initarg :window :initform 500 :reader hrauth-window)
   (cipher :initarg :cipher :initform #x00010001 :reader hrauth-cipher)
   (nickname :initarg :nickname :initform nil :reader hrauth-nickname)
   (timestamp :initarg :timestamp :initform 0 :reader hrauth-timestamp)))

;; --------------------------------------------

(defun aes-encrypt (key buf)
  (let ((rbuf (nibbles:make-octet-vector (length buf))))
    (ironclad:encrypt (ironclad:make-cipher :aes
					    :key key
					    :mode :cbc
					    :initialization-vector (nibbles:make-octet-vector 16))
		      buf
		      rbuf)
    rbuf))

(defun aes-decrypt (key buf)
  (let ((rbuf (nibbles:make-octet-vector (length buf))))
    (ironclad:decrypt (ironclad:make-cipher :aes
					    :key key
					    :mode :cbc
					    :initialization-vector (nibbles:make-octet-vector 16))
		      buf
		      rbuf)
    rbuf))

(defun sha1 (buf)
  (ironclad:digest-sequence :sha1 buf))

(defun sha1-hmac (buf key)
  (let ((ipad (nibbles:make-octet-vector 20))
	(opad (nibbles:make-octet-vector 20)))
    (dotimes (i 20)
      (setf (aref opad i) (logxor #x5c (aref key i))
	    (aref ipad i) (logxor #x36 (aref key i))))

    (let ((rhash (sha1 (concatenate '(vector (unsigned-byte 8))
				    ipad
				    buf))))
      (sha1 (concatenate '(vector (unsigned-byte 8))
			 opad
			 rhash)))))


;; --------------- client --------------

(defconstant +hrauth-cipher+ #x00010001)

(defun unix-time ()
  (- (get-universal-time) (encode-universal-time 0 0 0 1 1 1970)))

(defun make-hrauth-verf (key)
  (let* ((timestamp (unix-time))
	 (v (make-verf :nonce (random #xffffffff)
		       :timestamp timestamp
		       :tverf (1- timestamp)
		       :nickname 0))
	 (blk (drx:make-xdr-block)))
    (encode-verf blk v)
    (let ((buf (subseq (drx:xdr-block-buffer blk) 0 (drx:xdr-block-offset blk))))
      (aes-encrypt key buf)
      (frpc2:make-opaque-auth :flavour +hrauth+
			      :data (list buf 0 (length buf))))))


(defun make-hrauth-auth (full)
  (let ((blk (drx:make-xdr-block)))
    (encode-auth blk (drx:make-xunion +full+ full))
    (frpc2:make-opaque-auth :flavour +hrauth+
			    :data (list (drx:xdr-block-buffer blk) 0 (drx:xdr-block-offset blk)))))

(defun make-random-key ()
  (let ((buf (nibbles:make-octet-vector 32)))
    (dotimes (i 32)
      (setf (aref buf i) (random 256)))
    buf))

(defmethod frpc2:client-authenticate ((pvr hrauth-provider) msg)
  ;; return (values auth verf)
  (let* ((cred (make-cred :nonce (random #xffffffff)
			  :skey (make-random-key)
			  :service (hrauth-service pvr)
			  :window (hrauth-window pvr)
			  :cipher (hrauth-cipher pvr)))
	 (blk (drx:xdr-block 64)))
    (encode-cred blk cred)
    (let ((full (make-fullname :id (hrauth-localid pvr)
			       :cipher (hrauth-cipher pvr)
			       :ecred (drx:xdr-block-buffer blk))))
      (values (make-hrauth-auth full)
	      (make-hrauth-verf (getf cred :skey))))))

(defmethod frpc2:client-verify ((pvr hrauth-provider) verf)
  ;; signal error on failure, nil otherwise
  nil)

(defmethod frpc2:client-modify-call ((pvr hrauth-provider) blk start end)

  ;; (when (= (hrauth-service pvr) +service-none+)
  ;;   (return nil))
  
  ;;   ;; memmove args up by 32 bytes
  ;; (let* ((args (subseq buf start end))
  ;; 	 (hash (hrauth-digest args session-key cipher)))
    
  ;;     (dotimes (i (length args))
  ;; 	(setf (aref buf (+ start i)) (aref args i))))
    
  ;; ;; insert hash
  ;; (dotimes (i 32)
  ;;   (setf (aref buf (+ start i)) (aref hash i)))

  ;; ;; increment count
  ;; (incf (drx:xdr-block-offset blk) 32)

  ;; (when (= (hrauth-service pvr) +service-priv+)
  ;;   ;; encrypt
  ;;   nil)

  nil)

    

(defmethod frpc2:client-modify-reply ((pvr hrauth-provider) blk start end)
  nil)

;; ----------------- server -----------------

;; (defmethod frpc2:server-authenticate ((pvr hrauth-provider) auth verf msg blk)
;;   nil)

;; (defmethod frpc2:server-modify-call ((pvr hrauth-provider) cxt blk start end)
;;   nil)

;; (defmethod frpc2:server-modify-reply ((pvr hrauth-provider) cxt blk start end)
;;   nil)
