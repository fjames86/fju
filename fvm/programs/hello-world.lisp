
(defpackage #:hello-world
  (:use #:cl #:fvm)
  (:export #:hello-world))

(in-package #:hello-world)

(defword hello-world ()
  "Hello world!" dumpstr cr)

(defun hello-world ()
  (save-program "hello-world.fvm" 'hello-world))

