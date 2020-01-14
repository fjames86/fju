
(defpackage #:event-logger
  (:use #:cl #:fvm)
  (:export #:event-logger))

(in-package #:event-logger)

;; This program gets run when an event occurs and simply writes the event data to a log

(defword main () ;; (categoryH categoryL idH idL)
  ;; bos contains parm data, r0 contains parmsiz
  (push r0)
  
  >r  ;; save parmsize to return stack
  swap2
  xdr-encode-uint32 ;; category 
  xdr-encode-uint32 ;; eventid 
  bos r> xdr-encode-opaque ;; eventdata

  get-rpc-buffer get-rpc-offset write-output-binary)

(defun event-logger ()
  (save-program "event-logger.fvm" 'main))

