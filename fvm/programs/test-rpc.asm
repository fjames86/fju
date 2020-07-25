
;;; This file defines an example RPC program. it implements a few procedures
;;; which can be called from rpclt, e.g. to call PROC-COUNTER: 
;;; rpclt rawmode 2333333 1 3
	
	
	.MODULE		test-rpc
	.PROGRAM	2333333		1

	.INCLUDE	"native.asm"


	
;;; ------------------------------------------
	
PROC-NULL:
	PUSH		hello
	CALLNAT		NATIVELOGSTR
	SUBSP		4
	
	RET
	
;;; ------------------------------------------
	
	.DATA		hello		"Hello, world!\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"	
PROC-HELLO:
	LDSP		R0		-8    ; resp
	ST		R0		hello ; set result buffer
	LDSP		R0		-12   ; rescountp
	LD		R1		hello
	ADD		R1		4
	ST		R0		R1
	RET

;;; ------------------------------------------
	
PROC-ECHO:
	;; Echo args back to caller
	LDSP		R0		-8 ;resp
	LDSP		R1		-16 ; argp
	ST		R0		R1  ; set result buffer
	LDSP		R0		-12 ; rescount
	LDSP		R1		-20 ; argcount
	ST		R0		R1 
	RET

;;; ------------------------------------------
	
	.DATA		counter		0
PROC-COUNTER:
	;; increment value 
	LD		R1		counter
	ADD		R1		1	
	LDI		R2		counter
	ST		R2		R1

	;; return incremented value
	LDSP		R0		-8
	ST		R0		counter
	LDSP		R0		-12
	ST		R0		4

	RET
	
;;; ------------------------------------------



;;; ------- Export table ----------------------------------------------------
		
	.EXPORT		PROC-NULL	PROC
	.EXPORT		PROC-HELLO	PROC
	.EXPORT		PROC-ECHO	PROC
	.EXPORT		PROC-COUNTER	PROC
	.EXPORT		hello		STRING	
	
	
