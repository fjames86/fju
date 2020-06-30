
MEMSET:
	;; dest val count
	POP		R7
	POP		R0 	; count
	POP		R1	; val
	POP		R2	; dest
	CMP		R0	0 
	JZ		MEMSET-END
MEMSET-LOOP:
	STINC		R2	R1 ; store and increment 
	SUB		R0	4
	JP		MEMSET-LOOP
MEMSET-END:
	PUSH		R7
	RET
