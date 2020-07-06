
STRCPY:
	;; dest source count
	LDSP		R0	-4 ; count, size of dest buffer
	LDSP		R1	-8 ; source
	LDSP		R2	-12 ; dest

	;; get source string length. If larger than dest buffer truncate
	LD		R3	R1
	ADD		R3	4 ;add length prefix size 
	CMP		R3	R0
	JP		STRCPY-LOOP
	MOV		R3	R0	
STRCPY-LOOP:
	LDINC		R4	R1 ;get value from source 
	STINC		R2	R4 ;store into dest 
	SUB		R3	4
	JP		STRCPY-LOOP

	RET
	
