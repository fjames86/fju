
;;; memcpy routine
	
MEMCPY:
	LEASP 		R0	-4 	; count
	LD		R0	R0
	LEASP		R1	-8	; dest pointer
	LD		R1	R1
	LEASP		R2	-12	; source pointer
	LD		R2 	R2
	ADD		R0 	0 	; test count 
	JZ		MEMCPY-DONE
MEMCPY-START:	
	LD		R3	R2 	; load source value
	ST		R1	R3
	ADD		R2	4
	ADD		R1	4
	SUB		R0	4
	JP		MEMCPY-START
MEMCPY-DONE:
	ALLOCA		-12		; clean stack
	RET

	
