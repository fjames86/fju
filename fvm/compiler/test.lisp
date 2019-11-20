;;;; Copyright Frank James 2019

(defpackage #:fvm-test
  (:use #:cl #:fvm))

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
  tick-count dumphex cr
  loop)

(defword testtime ()
  time swap dumphex #\space dumpchr dumphex cr)

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
    write-output))

(let ((str "HelloWorld2"))
  (defword test-output2 ()
    (lisp str)
    (lisp (length str))
    write-output))

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
  halt)


(defword test-nohalt ()
  "NoHalt" dumpstr cr)

(let ((again (gensym)))
  (defword memcpy () ;; (dest-addr src-addr count --)
    (pop r0) ;; count
    (pop r1) ;; src-addr
    (pop r2) ;; dest-addr
    (ldi r4 0) ;; loop index 
    (lisp again) ;; start label
    (ldr r3 r1 0)
    (str r2 r3 0) ;; copy value
    (add r1 r1 1)
    (add r2 r2 1) ;; increment addresses
    (add r4 r4 1) ;; increment loop index 
    (cmp r5 r4 r0) ;; test index
    (lisp `((br-pn ,again)))))

(let ((again (gensym)))
  (defword memset () ;; (dest-addr val count --)
    (pop r0) ;; count
    (pop r1) ;; val
    (pop r2) ;; dest-addr
    (ldi r4 0) ;; loop index 
    (lisp again) ;; start label
    (str r2 r1 0) ;; copy value
    (add r2 r2 1) ;; increment address
    (add r4 r4 1) ;; increment loop index 
    (cmp r5 r4 r0) ;; test index
    (lisp `((br-pn ,again)))))


(defword test-callword () ;; (str -- num str)
  dumpstr cr ;; print the input string 
  #x0bad ;; push output number (num --)
  variable *input-buffer* dup ;; (num addr addr --)
  #x6565 12 memset ;; (num addr --)
  )


(defun test ()
  (save-program "test.obj" 'test-callword 
		:print-assembly t
		:variables '(*mystring* *input-buffer*)))

