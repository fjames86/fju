;;; This allows programs to register for a callback at some point later

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
	RET

	
	.EXPORT		REGISTER
	.EXPORT		SERVICE
