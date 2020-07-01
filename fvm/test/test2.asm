
;;; This is used to test cross-module calling using CALLVIRT instruction.
	.MODULE		TEST2
	.PROGRAM	10001		1
MAIN:
	LDI		R0		10000
	LDI		R1		0
	LDI		R2		0
	CALLVIRT	R0		R1		R2
	CMP		R2		0
	JPN		TEST-FAIL
	RET

TEST-FAIL:
	HALT
	
	.EXPORT		MAIN
