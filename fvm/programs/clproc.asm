
	.MODULE		CLPROC
	.PROGRAM	c173900a

	.CONST		MODULE-CLPROG	c1739009
	.CONST		CLPROG-entries	2
	.CONST		CLPROG-seqno	3
	.CONST		CLPROG-count	4
	
	.DATA		seqno		0
	
SERVICE:
	LD		R1	seqno
	PUSH		R1

	LDI		R2 	MODULE-CLPROG
	LDI		R3 	CLPROG-GET
	LDI		R4 	4 	
	CALLVIRT	R2	R3	R4
	;; R4 contains result length, stack contains list of progid/procid
SERVICE-LOOP:	
	CMP		R4	0
	JNZ		SERVICE-DONE

	CALL		INVOKE
	SUBSP		8 
	SUB		R4	8
	
	JMP		SERVICE-LOOP
	
SERVICE-DONE:
	LDI		R0	0
	LDI		R1	0
	RET
	
;;; -----------------------------------------------
	
	;; invoke(procid,progid)
INVOKE:
	LDSP		R0	-8 ;procid
	LDSP		R1	-12 ; progid
	LDI		R2 	0
	CALLVIRT 	R1	R0 	R2
	LDI		R0	0
	RET
	
;;; -----------------------------------------------
	
	.EXPORT		SERVICE
