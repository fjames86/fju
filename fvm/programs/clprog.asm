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
	
	;; TODO: return all entries starting from seqno
	RET

	.EXPORT		PROC-NULL
	.EXPORT		PROC-REGISTER
	.EXPORT		PROC-GET 
