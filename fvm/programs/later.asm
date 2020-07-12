;;; This allows programs to register for a callback at some point later.
;;; It has a registered service routine that gets called periodically. In this
;;; service routine it looks at any registered callback entries, works out if any
;;; need calling and then invokes them using CALLVIRT. Invoked entries are then removed
;;; from the list.

	.MODULE		later
	.PROGRAM	1a7e1a23	1

	;; struct entry {
	;; uint32_t progid, procid;
	;; uint64_t timeout;
	;; };	
	.CONST		entrysize	16
	.CONST		maxentry	16
	
	.DATA		entries		ARRAY 256
	.DATA		nentry		0
	
	;; register(progid,procid,timeout)
REGISTER:
	CMP		R0		entrysize
	JZ		L1
	LDI		R0		-1 ; bad args, cant continue
	JMP		DONE
L1:
	LDINC		R2		R1 ; progid
	LDINC		R3		R1 ; procid
	LDINC		R4		R1 ; timeout

	LDI		R1		entries
	LDI		R0		entrysize
	LD		R5		nentry
	MUL		R0		R5
	ADD		R1		R0 ; get offset to next entry slot

	;;  store new entry
	;; xxxx to do
DONE:	
	RET


	
	;; sevice routine (iterator). Execute any registered callbacks.
SERVICE:
	;; invoke all entries
	LD		R0		nentries
SERVICE-AGAIN:	
	SUB		R0		1
	JP		SERVICE-AGAIN
	
	RET


	;; invoke(entry *)
INVOKE:
	LDSP		R0	-8 ; get entry pointer
	LDINC		R1	R0 ; get progid
	LDIINC		R2 	R0 ; get procid
	LDI		R3 	0
	CALLVIRT	R1 	R2 	R3 
	RET

	;; int gettimeout()
GETTIMEOUT:	
	LDI		R0 		1000
	LD		R1		nentry ; loop counter
	LDI		R2		entries
	ADD		R2 		8 ; offset of timeout 
GETTIMEOUT-AGAIN:
	LD		R3		R2 ; get timeout
	CMP		R3 		R0 ; compare against current timeout
	JNZ 		GETTIMEOUT-SET
	ST		R0		R3
GETTIMEOUT-SET:		
	SUB		R1		1
	JP		GETTIMEOUT-AGAIN
	;;  return timeout in R0
	RET

	
	.EXPORT		REGISTER
	.EXPORT		SERVICE
