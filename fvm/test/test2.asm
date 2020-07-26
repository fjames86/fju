
;;; This is used to test cross-module calling using CALLVIRT instruction.
	.MODULE		TEST2
	.PROGRAM	10001		1

	.INCLUDE	"native.asm"
	
	.TEXT		test2-success	"Test2 Success\n"
	
MAIN:
	LDI		R0		10000
	LDI		R1		0
	LDI		R2		0
;;; CALLVIRT	R0		R1		R2 ; ;R2 contains arg length (0) and receives result length 
	CMP		R2		0
	JPN		TEST-FAIL

	PUSH		test2-success
	CALLNAT		NATIVEPUTS
	SUBSP		4
	
	LDI		R0		10000
	LDI		R1		1
	PUSH		123
	LDI		R2		4
;;; 	CALLVIRT	R0		R1		R2
	CMP		R2		4
	JPN		TEST-FAIL
	SUBSP		R2 	; clean stack 
	
	PUSH		test2-success
	CALLNAT		NATIVEPUTS
	SUBSP		4

	LDI		R0 		0
	LDI		R1		0 
	RET

TEST-FAIL:
	HALT
	
	.EXPORT		MAIN
