
MEMSET:
	;; dest val count
	LDSP		R0	-8 ; count
	LDSP		R1	-12 ; val
	LDSP		R2	-16 ; dest 
	CMP		R0	0 
	JZ		MEMSET-END
MEMSET-LOOP:
	STINC		R2	R1 ; store and increment 
	SUB		R0	4
	JP		MEMSET-LOOP
MEMSET-END:
	RET
