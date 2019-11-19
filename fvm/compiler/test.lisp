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
  variable *input-buffer* 64 read-input  
  dup ;; (msglen msglen --)
  if "GotAMessage Count=" dumpstr dumphex cr
  variable *input-buffer* dumpstr cr 
  else "GotNoMessage" dumpstr drop then)
  


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
  halt)


(defun test ()
  (save-program "test.obj" 'test
		:print-assembly t
		:variables '(*mystring* *input-buffer*)))
