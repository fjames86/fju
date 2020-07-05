
	.MODULE		test3

MAIN:
	ADDSP		4	;int i 
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
	
	PUSH		R0	
	PUSH		R1
	CALL		FN2	; R0 = FN2(n, &n)
	SUBSP		8 	; clean stack
	
	RET

;;; ----------------------------------------------------------------	

	;; void FN2(int x, int *y)
FN2:
	LDSP		R1		-8 ;get y
	LDSP		R0		-12 ;get x
	ADD		R0		1  ;x=x+1
	ST		R1		R0 ;*y = x
	RET
		
	.EXPORT	MAIN
	
