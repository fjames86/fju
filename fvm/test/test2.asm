
;;; This is used to test cross-module calling using CALLVIRT instruction.
	.MODULE		TEST2
	.PROGRAM	10001		1

	.INCLUDE	"native.asm"
	
	.TEXT		test2-success	"Test2 Success\n"
	
MAIN:
	PUSH		10000 	;progid
	PUSH		0	;procid
	PUSH		0	;args
	PUSH		0	;argcount
	PUSH		0	;res
	PUSH		0	;rescount
	CALLNAT		NATIVEINVOKE
	SUBSP		24 

	PUSH		test2-success
	CALLNAT		NATIVEPUTS
	SUBSP		4

	ADDSP		4	;allocate local
	LDI		R0		123
	STSP		R0		-4	
	PUSH		10000
	PUSH		1
	LEASP		R0		-12
	PUSH		R0
	PUSH		4
	PUSH		0
	PUSH		0
	CALLNAT		NATIVEINVOKE
	SUBSP		24
	SUBSP		4 

	PUSH		test2-success
	CALLNAT		NATIVEPUTS
	SUBSP		4

	LDI		R0 		0
	LDI		R1		0 
	RET

TEST-FAIL:
	HALT
	
	.EXPORT		MAIN
