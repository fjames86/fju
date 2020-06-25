
	.MODULE		test1
	.PROGRAM	10000	1

	.TEXT		hello	"Hello, world!"
	.TEXT		helloend
	
MAIN:	
	LDI		R0	10001
	LDI		R1	0
	LDI		R2	4
	PUSH 		12
	CALLVIRT	R0	R1	R2
	POP		R4

	LDI		R0	0
	LDI		R1	1
	LDI		R2	0
	PUSH		hello
	CALLVIRT	R0	R1	R2
	
	RET

	.EXPORT		MAIN
	
