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
	   #:! #:DUP2 #:AND #:2+ #:I #:J #:TUCK #:1+
	   #:XNOR #:NAND #:@ #:ROT #:NOR #:2- #:DROP #:FALSE
	   #:DUMPCHR #:ZERO #:SWAP #:HALT #:DUP #:OVER #:TRUE
	   #:TEST #:* #:- #:or #:xor #:+ #:mod #:/ #:not #:1-
	   #:dumpstr #:variable #:r> #:>r #:r@ #:tos #:bos #:zero!
	   #:lisp #:rand #:rti #:cr #:defisr #:compile-program
	   #:zero? #:!= #:break #:+loop #:dumphex #:lshift #:rshift
	   #:tick-count #:read-input #:reset-input #:write-output #:2dup #:nop
	   #:dumpstr-count #:memcpy #:memset #:dumpdec
	   #:call-load #:call-unload #:call-list #:call-pause #:strlen
	   #:defrpc #:xdr-reset #:xdr-encode-uint32 #:xdr-encode-uint64
	   #:xdr-encode-string #:xdr-encode-opaque #:xdr-encode-fixed
	   #:xdr-decode-uint32 #:xdr-decode-uint64 #:xdr-decode-string
	   #:xdr-decode-opaque #:xdr-decode-fixed #:rpc-call
	   #:xdr-encode-boolean #:xdr-decode-boolean #:write-output-binary
	   #:pprint-assembly #:get-rpc-timeout #:set-rpc-timeout
	   #:variable* #:get-rpc-service #:set-rpc-service
	   #:install-event-program #:install-startup-program
	   #:uint32 #:uint64 #:get-rpc-hostid #:set-rpc-hostid
	   #:print-program-script #:install-program
	   #:define-isr-table #:merge-isr-tables
	   #:fvm-id #:get-fvm-id #:strcmp #:local-variable
	   #:local #:local@ #:local! #:shmem #:strcpy #:strcat
	   #:swap2 #:over2 #:nth #:char@ #:char! #:i+
	   #:dumpstack #:i! #:set-rpc-buffer #:interrupt-service-loop
	   #:nth! #:fvm-shmem-read #:fvm-msg #:fvm-reset #:fvm-continue
	   #:fvm-pause #:goto #:get-rpc-offset #:.ARRAY 
	   ))
	   
