
	.MODULE		test-rpc
	.PROGRAM	2333333		1

PROC_NULL:
	RET

	.TEXT		hello		"Hello!"
HELLOEND:
	
PROC_HELLO:
	;; ignore all args
	LDI		R0		helloend
	LDI		R1		hello
	SUB		R0		R1		 ; R0 = buffer count
	LDI		R1		hello 		 ; R1 = buffer pointer
	RET

	.EXPORT		PROC_NULL
	.EXPORT		PROC_HELLO
	
	
