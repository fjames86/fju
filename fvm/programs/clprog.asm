;;; This allows running a procedure on all nodes in a cluster.
;;; Procedures are registered to be run by calling a method which
;;; simply writes into the (shared) data segment.
;;; A service routine will periodically poll this and run any that it finds.
;;; It works by cooperation between two modules - a clustered program
;;; that provides a single method for registering a procedure, and a
;;; non-clustered module with a service routine that polls the shared
;;; data for new entries.

	.MODULE		CLPROG		
	.PROGRAM	c1739009

	.CONST		maxentry	32 
	.CONST		entrysize	8

	; maxentry*entrysize		
	.DATA		entries		ARRAY		256 
	.DATA		seqno		0
	.DATA		count 		0
	
PROC-NULL:
	LDI		R0		0
	LDI		R1		0
	RET

	;; progid,procid
PROC-REGISTER:
	CMP		R0		8  ; check arg length
	JZ		L1		
	HALT
L1:	
	LDINC		R3		R1 ; progid
	LDINC		R4		R1 ; procid

	LD		R5		seqno
	MOD		R5		maxentry ; get index into array
	MUL		R5		entrysize
	LDI		R6		entries
	ADD		R6		R5 	; get offset into entry array 
	STINC		R5		R3	 ; set progid
	STINC		R5		R4	 ; set procid

	;; increment seqno 
	LDI		R5		seqno 
	INC		R5		1	

	;; increment count 
	LD		R5		count
	ADD		R5 		1
	MOD		R5		maxentries
	LDI		R6		count
	ST		R6		R5

	;; set return value
	LDI		R0		4
	LDI		R1		seqno 
	RET

	;; procget(seqno)
PROC-GET:
	CMP		R0		4 ; check args 
	JZ		$+4
	HALT
	
	;;  get starting index
	CALL		GETINDEX
	SUBSP		4

	CMP		R0		-1
	JPN		PROC-GET-L1
	LDI		R0		0
	LDI		R1		0
	RET
	
PROC-GET-L1:	

	;; R0 contains starting index, return count from there
	LD		R1		count
PROG-GET-LOOP:
	LDI		R2		entries
	ADD		R2		R0
	LDINC		R3		R2
	PUSH		R3
	LDINC		R3		R2
	PUSH		R3

	ADD		R0		1
	CMP		R0		R1
	JPN		$+4
	MOD		R0		R1 
	
	SUB		R1		1
	CMP		R1		0
	JP		PROG-GET-LOOP


	

PROC-GET-DONE:	
	RET

;;; --------------------------------------------------------------

	;; int getindex(seqno)
GETINDEX:
	;; Compute index into entries array given a seqno.
	;; returns -1 if seqno out of range

	;; get count
	LD	R0	count

	;;  get input seqno
	LDSP	R1	-8

	;; get current seqno
	LD	R2	seqno

	;; compare difference against count.
	;; if input - actual < 0 return -1
	;; if input - actual > count return -1
	;; else return input % max

	;; if(input > seqno) return -1
	CMP	R1	R2
	JNZ	$+8
	LDI	R0	-1
	RET

	;; if(input - seqno > count ) return -1
	MOV	R3	R2
	SUB	R3	R1
	CMP	R3	R0
	JNZ	$+8
	LDI	R0	-1
	RET

	MOD	R1	R0
	MOV	R0	R1
	RET

;;; --------------------------------------------------------------
	
	.EXPORT		PROC-NULL
	.EXPORT		PROC-REGISTER
	.EXPORT		PROC-GET 
