 ;;;; Copyright Frank James 2019

(defpackage #:fvm-test
  (:use #:cl #:fvm)
  (:export #:build-all))

(in-package #:fvm-test)

;; test a do loop 
(defword test-count ()
  26 0 do 65 i + dumpchr loop)

;; define some global variable 
(defvariable *mystring* "Hello, world!")
(defword hello-world ()
  variable *mystring* dumpstr)

;; test using lisp evaluation
(defun myfnbody (reg)
  `((push ,reg)
    (pop ,reg)))

(defword testfnbody ()
  123
  (lisp (myfnbody 'r0)))

;; test inserting jumps 
(let ((glabel (gensym)))
  (defword testjmp ()
    (ldi r0 -1)
    (lisp `((br-z ,glabel)))
    hello-world cr
    (lisp glabel)
    hello-world cr))

;; test random number generator
(defword testrand ()
  10 0
  do
    rand 26 mod #\A + dumpchr
  loop)

;; test illegal opcode interrupt 
(defword testint ()
  (fvm::res3)
  hello-world cr)

(defword testbreak ()
  1000 0 do
  rand 16 mod zero? if break then
  #\A dumpchr cr 
  loop)

(defword +looptest ()
  1000 0 do
  i dumphex cr
  rand 100 mod
  +loop)

(defword ticktest ()
  20 0 do
  tick-count dup dumphex " " dumpstr dumpdec cr
  loop)

(defword testtime ()
  time swap dumphex dumphex cr)

(let ((octets (do ((bytes (coerce (babel:string-to-octets "hello world") 'list)
			  (cddr bytes))
		   (ret nil))
		  ((null bytes) (nreverse ret))
		(push (logior (ash (or (cadr bytes) 0) 8)
			      (or (car bytes) 0))
		      ret)))
      (gstrbuf (gensym))
      (glbl (gensym)))
  (defword test-output ()
    (lisp `((br-pnz ,glbl)
	    ,gstrbuf 
	    (.blkw ,@octets)
	    ,glbl
	    (lea r0 ,gstrbuf)
	    (push r0)
	    (ldi r0 ,(* 2 (length octets)))
	    (push r0)))
    write-output-binary))

(defword test-output2 ()
  bobby 
  "HelloWorld2" write-output)

(defvariable *input-buffer* 0 32) ;; allocate 32 words (64 bytes) buffer space
(defword test-input ()
  begin 
    variable *input-buffer* 64 read-input  
    dup ;; (msglen msglen --)
    if "GotAMessage Count=" dumpstr dup dumphex cr
    variable *input-buffer* swap dumpstr-count cr true
    else "GotNoMessage" dumpstr drop false then
  until
  reset-input)

(defword test-sleep ()
  "Start " dumpstr 2000 sleep "End" dumpstr cr)

(defword test-strlen ()
  "123412345" strlen dumpdec cr)

(defrpc rpcbind-call-null (100000 2 0)
  :result-body ("rpcbind.null Success" dumpstr cr)
  :fail-body ("rpcbind.null failed" dumpstr cr))

(defrpc rpcbind-call-getport (100000 2 3)
  :result-body (xdr-decode-uint32 "Port = " dumpstr dumpdec drop cr)
  :fail-body ("rpcbind-call-getport failed" dumpstr cr))

(defrpc rpcbind-call-list (100000 2 4)
  :fail-body ("rpc-call-list failed" dumpstr cr)
  :result-body (
   "rpcbind-list:" dumpstr cr 
   "PROG     VERS     PROT     PORT" dumpstr cr 
   begin
     xdr-decode-boolean
     if
       4 0 do 
         xdr-decode-uint32 swap dumphex dumphex " " dumpstr 
       loop
       cr 
       true
     else
       false
     then
   until))
  
(defword rpcbind-call-list-remote ()
  uint64 #xb8ec6497ad281d4 set-rpc-hostid
  "rpcbind-call-list remote" dumpstr cr 
  rpcbind-call-list
  uint64 0 set-rpc-hostid)

(defrpc %freg-call-getbyname (#x27E1FB10 1 5))
#+nil(defword freg-call-get-string () ;; (name -- sts &optional val)
  xdr-reset
  xdr-encode-string
  0 3 xdr-encode-uint32 ;; flags=freg_type_string 
  %freg-call-getbyname
  if 
    xdr-decode-uint32 ;; get status
    if
      bos 64 xdr-decode-string bos dumpstr cr
    else
      "freg-get-by-name failed" dumpstr cr 
    then
  else
    "rpccall failed" dumpstr cr
  then)

  

(defword test-rpc ()
  rpcbind-call-null
  rpcbind-call-getport
  rpcbind-call-list)

(defword test-nested-if () ;; (f2 f1 --)
  if
    "first-true " dumpstr 
    if "second-true " else "second-false" then dumpstr
  else
    "first-false " dumpstr 
    if "second-true" else "second-false" then dumpstr
  then)

(defword test-nested-do ()
  5 0 do
    5 0 do
      "I=" dumpstr i dumpdec " J=" dumpstr j dumpdec cr
    loop
  loop)
  
(defword test-rpc-timeout ()
  "rpc-timeout = " dumpstr get-rpc-timeout dumpdec cr 
  1000 set-rpc-timeout
  "rpc-timeout = " dumpstr get-rpc-timeout dumpdec)

(defword test-rpc-service ()
  "rpc-service = " dumpstr get-rpc-service dumpdec cr 
  2 set-rpc-service
  "rpc-service = " dumpstr get-rpc-service dumpdec)

;; try a few words 
(defword test ()
  "hello-world: " dumpstr hello-world cr
  "test-count: " dumpstr test-count cr
  "testrand: " dumpstr testrand cr 
  "testbreak: " dumpstr testbreak cr 
  "+looptest: " dumpstr +looptest cr
  "ticktest: " dumpstr ticktest cr
  "testtime: " dumpstr testtime cr
  "test-output: " dumpstr test-output cr
  "test-output2: " dumpstr test-output2 cr
  "test-input: " dumpstr test-input cr
  "test-input: " dumpstr test-input cr
  "test-sleep: " dumpstr test-sleep cr
  "test-strlen: " dumpstr test-strlen cr
  "test-rpc: " dumpstr test-rpc cr
  "test-rpc-remote: " dumpstr rpcbind-call-list-remote cr
  "test-nested-if true true: " dumpstr true true test-nested-if cr 
  "test-nested-if true false: " dumpstr true false test-nested-if cr 
  "test-nested-if false true: " dumpstr false true test-nested-if cr
  "test-nested-if false false: " dumpstr false false test-nested-if cr
  "test-nested-do: " dumpstr test-nested-do cr
  "test-rpc-timeout: " dumpstr test-rpc-timeout cr
  "test-rpc-service: " dumpstr test-rpc-service cr)



(defword test-nohalt ()
  "NoHalt" dumpstr cr)

(defword test-callword () ;; (str -- num str)
  dumpstr cr ;; print the input string 
  #x0bad ;; push output number (num --)
  variable *input-buffer* dup ;; (num addr addr --)
  #x4141 12 memset ;; (num addr --)
  )

(defword infinite-loop ()
  begin true until)



(defun test ()
  (save-program "test.obj" 'test
		:print-assembly nil
		:variables '(*mystring* *input-buffer*)
		:extra-words '(test-callword)))


(defun test-call-start (&optional (entry-word 'test) autounload-p)
  (let ((progdata (compile-program entry-word
				   :variables '(*mystring* *input-buffer*)
				   :extra-words '(test-callword infinite-loop))))
    (call-load progdata :autounload-p autounload-p)))


;; --------------

(defword test-event-trigger ()
  "test-event-trigger" write-output)

(defun test-event-trigger ()
  (save-program "test-event-trigger.obj" 'test-event-trigger))


(defrpc call-cmdprog-event (999999 1 2)
  :arg-body ;; (eventid category --)
  (xdr-encode-uint32 xdr-encode-uint32)
  :result-body
  ("call-cmdprog-event success" dumpstr)
  :fail-body
  ("call-cmdprog-event failed" dumpstr))

(defword trigger-1234-event ()
  uint32 1234 uint32 1234 call-cmdprog-event)



  
;; -------------------------------

(define-isr-table *test-isr-table*)
(defisr test-msg-handler (*test-isr-table* #x03)
  ;; Get params R0 = msgid R1 = msgaddr R2 = msglen

  ;; for now just log the message 
  (push r1)
  write-output)

;; loop infinitely waiting to service an incoming message 
(defword test-msg-handler-loop ()
  begin
  1000 sleep
  true until)

(defun test-msg ()
  (save-program "test-msg.obj" 'test-msg-handler-loop :isr-table *test-isr-table*))

(defconstant +fvm-prog+ #x27E1FB11)
(defrpc call-send-msg (+fvm-prog+ 1 6)
  :arg-body (xdr-encode-uint32 xdr-encode-uint32 xdr-encode-string)
  :result-body ("call-send-msg success" dumpstr)
  :fail-body ("call-send-msg failed" dumpstr))

(defrpc fvm-list-all (+fvm-prog+ 1 3)
  :result-body 
  (xdr-decode-boolean
   begin
   "id=" dumpstr xdr-decode-uint32 dumphex                ;; id
   " flags=" dumpstr xdr-decode-uint32 swap dumphex dumphex ;; flags
   xdr-decode-uint64 (add sp sp 4) ;; drop drop drop drop ;; tickcount
   xdr-decode-uint64 (add sp sp 4) ;; drop drop drop drop ;; runtime
   xdr-decode-uint64 (add sp sp 4) ;; drop drop drop drop ;; inlogid
   xdr-decode-uint64 (add sp sp 4) ;; drop drop drop drop ;; outlogid
   " name=" dumpstr bos 1024 xdr-decode-string bos dumpstr
   cr 
   xdr-decode-boolean
   until))

(defword wait-for-logmsg ()
  begin
  bos 1024 read-input ;; (msglen)
  dup zero? ;; (msglen f)
  if ;; msglen = 0
    drop       ;; drop the msglen from stack
    1000 sleep ;; sleep for a while 
    true       ;; continue looping 
  else
    bos swap false  ;; (addr msg f) exit loop
  then
  until)

(defword test-logmsg ()
  begin
    wait-for-logmsg ;; wait until message received
    drop ;; ignore msglen
    dumpstr ;; print msg to console
  true until)

(defun test-logmsg ()
  (save-program "test-logmsg.obj" 'test-logmsg))

(defword small-test ()
  "hello world!" dumpstr cr)

(defword idtest ()
  "fvm-id: " dumpstr fvm-id swap dumphex dumphex cr)


;; --------------------------------------

(defparameter *build-programs* '(test test-msg test-logmsg))
(defun build-all ()
  (dolist (p *build-programs*)
    (funcall p)))
