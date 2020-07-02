
;;; This file defines an example RPC program. it implements a few procedures
;;; which can be called from rpclt, e.g. to call PROC-COUNTER: 
;;; rpclt rawmode 2333333 1 3
	
	
	.MODULE		test-rpc
	.PROGRAM	2333333		1

	.INCLUDE	"native.asm"
	
PROC-NULL:
	PUSH		hello
	CALLNAT		R2 		NATIVE-LOGSTR
	RET
	
PROC-HELLO:
	;; ignore all args
	SUBSP		R0
	
	LDI		R0		helloend
	LDI		R1		hello
	SUB		R0		R1		 ; R0 = buffer count
	LDI		R1		hello 		 ; R1 = buffer pointer
	RET

PROC-ECHO:
	;; Echo args back to caller
	SUBSP		R0	; clear stack 
	LEASP		R1		0  ; set r1 to buffer, r0 is count but that was already set for us	
	RET

	.DATA		counter		0
PROC-COUNTER:
	;; increment value 
	LD		R1		counter
	ADD		R1		1	
	LDI		R2		counter
	ST		R2		R1

	;; return incremented value 
	LDI		R1		counter
	LDI		R0		4
	RET

	.DATA		hello		"Hello, world!"
HELLOEND:


;;; ------- Export table ----------------------------------------------------
		
	.EXPORT		PROC-NULL	PROC
	.EXPORT		PROC-HELLO	PROC
	.EXPORT		PROC-ECHO	PROC
	.EXPORT		PROC-COUNTER	PROC
	.EXPORT		hello		STRING	
	
	
