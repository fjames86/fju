
STRCPY:
	;; dest source count
	POP		R7
	POP		R0	; count, size of dest buffer
	POP		R1	; source
	POP		R2	; dest

	;; get source string length. If larger than dest buffer truncate
	LD		R3	R1 
	CMP		R3	R0
	JPN		STRCPY-LOOP
	LDI		R3	R0	
STRCPY-LOOP:
	LDINC		R4	R1 ;get value from source 
	STINC		R2	R4 ;store into dest 
	SUB		R3	4
	JP		STRCPY-LOOP
	
	PUSH		R7
	RET
	
