
	.MODULE		test-rpc
	.PROGRAM	2333333		1

	.TEXT		hello		"Hello, world!"
HELLOEND:

PROC-NULL:
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
	LEASP		R1		R0
	RET
	
	.EXPORT		PROC-NULL
	.EXPORT		PROC-HELLO
	.EXPORT		PROC-ECHO
	
	
