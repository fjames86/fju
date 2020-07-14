
;;; ---------------------------------------------------
	
	.MODULE		test-service
	.PROGRAM	23333332	1

;;; ---------------------------------------------------
	
	.INCLUDE	"native.asm"

;;; ---------------------------------------------------

	.TEXT		progname	"test-rpc"
	.TEXT		logmsg		"Test service routine"

	.DATA		progid		0

;;; -----------------------------------------------------
	
SERVICE:
	
;; 	PUSH		logmsg
;; 	CALLNAT		R3		NATIVE-LOGSTR

	LD		R3		progid
	CALLZ		GETPROGID
	
	LD		R0		progid 
	LDI		R1		0 ; PROC-NULL
	LDI		R2		0 ; arg length 
	CALLVIRT	R0		R1		R2

	;; ret 
	LDI		R0		0
	LDI		R1		0
	RET

;;; -----------------------------------------------------

	.TEXT		yieldmsg	"Yield message"
	
INIT:
	CALL		GETPROGID

	PUSH		YIELD-FORK ; fork execution - continue 
	PUSH		5000	   ; timeout - child is executed after 5000ms 
	CALLNAT		R0		NATIVE-YIELD
	CMP		R0		1 ; R0 receives 1 in child, 0 in parent 
	JPN		noyield 

	PUSH		yieldmsg
	CALLNAT		R0		NATIVE-LOGSTR 
	
noyield:	
	LDI		R0		0
	LDI		R1		0
	RET

;;; -----------------------------------------------------
	
GETPROGID:
	POP		R7	; save return  address
	
	PUSH		progname
	CALLNAT		R3 		NATIVE-GETPROGID
	POP		R3	; get progid for module test-rpc
	LDI		R4		progid
	ST		R4		R3

	PUSH		R7
	RET

;;; ---------------------------------------------------
	
	.EXPORT		SERVICE
	.EXPORT		INIT
	
