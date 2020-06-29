
JMPREL:
	LDI		R0	JMPREL
	LEA		R1	0
	SUB		R1	R0
	JMP		R1
	
	;; dest source count 
MEMCPY:
	POP		R0 	; count
	POP		R1	; source
	POP		R2	; dest
MEMCPY-L1:
	LD		R3		R1
	ST		R2		R3
	ADD		R1		4
	ADD		R2		4
	SUB		R0		4
	JP		MEMCPY-L1
	RET
MEMCPY-END:
	.EXPORT		MEMCPY		PROC	MEMCPY-END

	;; dest val count
MEMSET:
	POP		R0	; count
	POP		R1	; val
	POP		R2	; dest
MEMSET-L1:
	ST		R2	R1
	ADD		R2	4
	SUB		R0	4
	JP		$-MEMSET-L1
	RET
MEMSET-END:	
	.EXPORT		MEMSET		PROC	MEMSET-END

	;; dest source
STRCPY:
	POP	R0		; source
	POP	R1		; dest
	LD	R2	R1	; get source length
	ST	R1	R2	; store length
	ADD	R1	4	
STRCPY-L1:
	LD	R3	R0
	ST	R1	R3
	ADD	R0	4
	ADD	R1	4
	SUB	R2	4
	JP	$-STRCPY-L1
	RET
STRCPY-END:
	.EXPORT		STRCPY		PROC	STRCPY-END


	
	
