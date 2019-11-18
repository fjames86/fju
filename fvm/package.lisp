;;;; Copyright Frank James 2019
;;;; This code is licensed under the MIT license.

(defpackage #:fvm
  (:use #:cl)
  (:export #:r0 #:r1 #:r2 #:r3 #:r4 #:r5 #:r6 #:r7 #:sp #:rp
	   #:defword #:defvariable
	   #:save-program
	   #:begin #:until #:else #:then
	   #:.blkw #:.origin #:.orig #:.string
	   #:br #:br-p #:br-z #:br-n #:br-pn #:br-np #:br-pz #:br-zp 
	   #:br-nz #:br-zn #:br-pnz #:br-pzn #:br-npz #:br-nzp #:br-zpn #:br-znp
	   #:add #:ld #:st #:call #:nand #:ldr #:str #:push #:rpush #:pop #:rpop
	   #:ldi #:sti #:jmp #:ret #:mul #:div #:mod #:cmp #:lea 
	   #:! #:DUP2 #:AND #:2+ #:I #:TUCK #:1+
	   #:XNOR #:NAND #:@ #:ROT #:NOR #:2- #:DROP #:FALSE
	   #:DUMPCHR #:ZERO #:SWAP #:HALT #:DUP #:OVER #:TRUE
	   #:TEST #:* #:- #:or #:xor #:+ #:mod #:/ #:not #:1-
	   #:dumpstr #:variable #:r> #:r< #:r@ #:tos #:bos #:zero!
	   #:lisp #:rand #:rti #:cr #:defisr #:compile-program
	   #:zero? #:!=))
	   
   
