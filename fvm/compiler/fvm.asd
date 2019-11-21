;;;; Copyright (c) Frank James 2019 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(asdf:defsystem :fvm
  :name "fvm"
  :author "Frank James <frank.a.james@gmail.com>"
  :description "Forth compiler for FVM"
  :license "MIT"
  :serial t
  :components
  ((:file "package")
   (:file "fvm")
   (:file "words")
   (:file "rpc"))
  :depends-on (:babel :nibbles :frpc2))

