
	.MODULE		test3

MAIN:
	LEA		R0		8
	PUSH		R0
	JMP		subfn
	RET

subfn:
	POP		R0
	JMP		R0
	
	.EXPORT	MAIN
	
