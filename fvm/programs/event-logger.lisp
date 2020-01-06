
(defpackage #:event-logger
  (:use #:cl #:fvm))

(in-package #:event-logger)

;; This program gets run when an event occurs and simply writes the event data to a log

(defword buf (:gensyms (gdone))
  (lea r0 5)
  (push r0)
  (br-pnz 1)
  (.blkw gdone)
  (ld r0 -2)
  (jmp r0)
  (.ARRAY 1024)
  gdone)

(defword main () ;; (categoryH categoryL idH idL)
  ;; bos contains parm data, r0 contains parmsiz
  (push r0)
  
  ;; set rpc device buffer to encode log message  
  1024 buf set-rpc-buffer

  >r  ;; save parmsize to return stack
  swap2
  xdr-encode-uint32 ;; category 
  xdr-encode-uint32 ;; eventid 
  bos >r xdr-encode-opaque ;; eventdata

  bos get-xdr-offset write-output-opaque)


  
  
