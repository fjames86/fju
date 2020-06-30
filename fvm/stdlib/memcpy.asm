
;;; Include this file to add a memcpy function
MEMCPY:
	;; dest source count
	POP		R7
	POP		R0	; count
	POP		R1	; source
	POP		R2	; dest
	CMP		R0	0
	JZ		MEMCPY-END
MEMCPY-L1:
	LDINC		R3	R1 ; load [R1] into R3, set R1+=4
	STINC		R2	R3
	SUB		R0	4
	JP		MEMCPY-L1
MEMCPY-END:
	PUSH		R7 	;restore return address 
	RET


	
