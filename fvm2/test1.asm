
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

	PUSH		hello
	CALL		PUTS
	
	RET


PUTS:
	POP		R7	   	; save return address
	POP		R0		; get address of string
	LD		R1	R0	; get length of string
	MOV		R6	R1	; save length 
	ADD		R0	4
	PUSH		R1		; push length
	ADD		R1	0 	; test length 
	JZ		PUTS_DONE
PUTS_LOOP:	
	LD		R2	R0
	PUSH		R2
	ADD		R0	4
	SUB		R1	4
	JP		PUTS_LOOP
	
PUTS_DONE:
	LDI		R0 	0
	LDI		R1	1
	ADD		R6	4
	CALLVIRT	R0	R1	R6
	
	PUSH 		R7		; restore return address
	RET
	
	
	.EXPORT		MAIN
	
