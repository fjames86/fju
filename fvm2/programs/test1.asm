
	.MODULE		test/rpc
	.PROGRAM	2333333		1

PROC_NULL:
	RET

	.TEXT		hello		"Hello!"
PROC_HELLO:
	;; ignore all args
	LDI		R1		0
	SUB		R1		R0
	ALLOCA		R1	; clear args from stack

	;; copy hello string to stack
	LEASP		R1		0
	PUSH		R1	; save stack pointer
	
	LEASP		R1
	PUSH		R1
	LDI		R1 		hello
	PUSH 		R1
	LDI		R1		12
	CALL		MEMCPY

	;; set r0 to result length
	POP		R1	; get stack pointer before
	LEASP		R0		0 ; subtract from current stack pointer 
	SUB		R0		R1
	RET

	.EXPORT		PROC_NULL
	.EXPORT		PROC_HELLO
	
	
