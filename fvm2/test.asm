
	.MODULE helloworld
	
	.TEXT 	hello	"Hello, World!"
MAIN:	
	LDI	R1	hello
	XOR	R2	R2
	ADD	R2	12
	MUL	R2 	2
	MOV	R3	R2
	MOD	R3	5
	ADD	R3	1
	PUSH 	14
	CALL	FN1
	RET

FN1:
	POP	R2		; save return address
	POP	R1
	ADD	R1 	2
	PUSH 	R1
	PUSH 	R2
	RET
	
	.EXPORT	MAIN
	
