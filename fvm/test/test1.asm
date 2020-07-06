
;;; This defines some tests that can be run as part of the build.
;;; We expect this to build and run without errors.

	.MODULE		TEST1
	.PROGRAM	10000		1

	.INCLUDE	"native.asm"
	
	.TEXT		test-success 	"Test1 success\n"
	
MAIN:
	PUSH		3
	PUSH		2
	PUSH 		5
	CALL		TEST-ADD

	PUSH		8
	PUSH		3
	PUSH		5
	CALL		TEST-SUB

	PUSH		3
	PUSH		9
	PUSH		27
	CALL		TEST-MUL

	PUSH		21
	PUSH		10
	PUSH		1
	CALL		TEST-MOD

	PUSH		21
	PUSH		7
	PUSH		3
	CALL		TEST-DIV

	PUSH		test-success
	CALLNAT		R6	NATIVE-PUTS
	SUBSP		4
	
	;; Set result registers
	LDI		R0		0
	LDI		R1		0
	RET

	.TEXT		test-add-name "TEST-ADD\n"
TEST-ADD:
	PUSH		test-add-name
	CALLNAT		R6	NATIVE-PUTS
	SUBSP		4
	
	POP		R7
	POP		R0	; get expected result
	POP		R1	; get y
	POP 		R2	; get x
	ADD		R2	R1
	CMP		R2	R0
	JPN		TEST-FAIL
	PUSH		R7
	RET

	.TEXT		test-sub-name 	"TEST-SUB\n"
TEST-SUB:
	PUSH		test-sub-name
	CALLNAT		R6	NATIVE-PUTS
	SUBSP		4
	
	POP		R7
	POP 		R0
	POP		R1
	POP		R2
	SUB		R2	R1
	CMP		R2	R0
	JPN		TEST-FAIL
	PUSH		R7
	RET
	
	.TEXT		test-mul-name 	"TEST-MUL\n"
TEST-MUL:
	PUSH		test-mul-name
	CALLNAT		R6	NATIVE-PUTS
	SUBSP		4
	
	POP		R7
	POP 		R0
	POP		R1
	POP		R2
	MUL		R2	R1
	CMP		R2	R0
	JPN		TEST-FAIL
	PUSH		R7
	RET

	.TEXT		test-mod-name 	"TEST-MOD\n"
TEST-MOD:
	PUSH		test-mod-name
	CALLNAT		R6	NATIVE-PUTS
	SUBSP		4
	
	POP		R7
	POP 		R0
	POP		R1
	POP		R2
	MOD		R2	R1
	CMP		R2	R0
	JPN		TEST-FAIL
	PUSH		R7
	RET

	.TEXT		test-div-name 	"TEST-DIV\n"
TEST-DIV:
	PUSH		test-div-name
	CALLNAT		R6	NATIVE-PUTS
	SUBSP		4
	
	POP		R7
	POP 		R0
	POP		R1
	POP		R2
	DIV		R2	R1
	CMP		R2	R0
	JPN		TEST-FAIL
	PUSH		R7
	RET
	

TEST-FAIL:
	HALT
	

TEST-ADDPROC:
	;;  R0 = length R1 = pointer 
	LD		R2		R1 ; get a number 
	ADD		R2		1
	ST		R1		R2 ; set results
	;; R0 = length (unchanged) R1 = pointer (unchanged)
	RET
	
	.EXPORT		MAIN
	.EXPORT		TEST-ADDPROC
	.EXPORT		TEST-ADD
	
	
