
;;; Include this file to add a memcpy function
MEMCPY:
	;; dest source count
	LDSP		R0	-4 ; count
	LDSP		R1	-8 ; source
	LDSP		R2	-12 ; dest
	CMP		R0	0
	JZ		MEMCPY-END
L1$:
	LDINC		R3	R1 ; load [R1] into R3, set R1+=4
	STINC		R2	R3
	SUB		R0	4
	JP		L1$
MEMCPY-END:
	RET


	
