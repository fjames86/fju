
	.MODULE 	test2
	.PROGRAM	10001 	1

MAIN:
	CMP		R0	4 ; check arg length
	JZ		DONE	
	POP		R1	; get arg from stack

	;; do some work (e.g. add 1)
	ADD		R1	1

	;; push result into stack and set r0 to size of results 
	PUSH		R1
	LDI		R0	4
DONE:	
	RET


	.EXPORT		MAIN
	
