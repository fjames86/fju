
;;; memcpy routine
	
MEMCPY:
	POP		R7	   ; save return address
	POP 		R0	   ; count
	POP		R1	   ; dest
	POP		R2	   ; source
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
	PUSH		R7
	RET

	
