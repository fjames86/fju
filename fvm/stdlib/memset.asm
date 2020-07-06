
MEMSET:
	;; dest val count
	LDSP		R0	-8 ; count
	LDSP		R1	-12 ; val
	LDSP		R2	-16 ; dest 
	CMP		R0	0 
	JZ		MEMSET-END
L1$:	
	STINC		R2	R1 ; store and increment 
	SUB		R0	4
	JP		L1$
MEMSET-END:
	RET
