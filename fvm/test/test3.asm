
;;; This file shows a loop and calling convention cdecl
;;; Calling convention is:
;;;  - push args in reverse order
;;;  - call. on return r0 contains return result value
;;;  - clean stack by subtracting length of args pushed
;;;
	
	
	.MODULE		test3

MAIN:
	ADDSP		4	;reserve some space for an int i 
	LDI		R0		0
	STSP		R0		-4 ; set i = 0
LOOP:	
	LDSP		R0		-4 ; get i
	
	PUSH		R0
	CALL 		FN1	; R0 = FN1(i)
	SUBSP		4	; clean stack 
	
	STSP		R0		-4 ; i = FN1(i)
	
	CMP		R0		12 ; if(i < 12) 
	JN		LOOP		   ; goto loop 
	
	RET

;;; ----------------------------------------------------------------
	
	;; int FN1(int n) 
FN1:
	LDSP		R0	-8 ;get n
	LEASP		R1	-8 ;&n
	
	PUSH		R1	
	PUSH		R0
	CALL		FN2	; R0 = FN2(n, &n)
	SUBSP		8 	; clean stack
	
	RET

;;; ----------------------------------------------------------------	

	;; void FN2(int x, int *y)
FN2:
	LDSP		R0		-8 ;get x
	LDSP		R1		-12 ;get y
	ADD		R0		1  ;x=x+1
	ST		R1		R0 ;*y = x
	RET

;;; ----------------------------------------------------------------

	.INCLUDE	"native.asm"
	
	.TEXT		testlogname 	"test3"
	.TEXT		bufp 		0x12345678
testlogging:
	PUSH		4
	PUSH		bufp	
	PUSH		testlogname	
	CALLNAT		R0		NATIVE-WRITELOG
	SUBSP		12

	PUSH		4
	PUSH		bufp
	PUSH		0
	PUSH		0	
	PUSH		testlogname
	CALLNAT		R0		NATIVE-READLOG
	SUBSP		16

	RET


infiniteloop:
	NOP
	JMP 		infiniteloop
	RET
	
	.EXPORT		MAIN
	.EXPORT		testlogging
	.EXPORT 	infiniteloop
	
