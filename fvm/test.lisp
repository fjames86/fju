
(defpackage #:fvm-test
  (:use #:cl #:fvm))

(in-package #:fvm-test)

(defword test-count ()
  26 0 do 65 i + dumpchr loop)

(defvariable *mystring* "Hello, world!")
(defword hello-world ()
  variable *mystring* dumpstr)

(defword cr ()
  10 dumpchr)

(defword test ()
  hello-world cr
  test-count cr 
  halt)

(defun myfnbody (reg)
  `((push ,reg)
    (pop ,reg)))

(defword testfnbody ()
  123
  (lisp (myfnbody 'r0)))

(let ((glabel (gensym)))
  (defword testjmp ()
    (ldi r0 -1)
    (lisp `((br-z ,glabel)))
    hello-world cr
    (lisp glabel)
    hello-world cr
    halt))

(defword testrand ()
  10 0 do
    rand 26 mod #\A + dumpchr cr
    loop)

  
(defun test ()
  (save-program "test.obj" 'testrand
		:print-assembly t
		:variables '(*mystring*)))

