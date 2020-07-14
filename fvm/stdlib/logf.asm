
;;; This provides a mechanism for logging a formatted message string

	.INCLUDE	"native.asm"

	.IFDEF		%LOGF
	
	;; logf( fmt, .... ) */
LOGF:
	POP		R7	  ; save return address
	
	PUSH		256 	; length
	LEASP		R1	4 ; set buffer to current stack pointer
	PUSH		R1	; buffer
	CALLNAT		R0	NATIVE-SPRINTF ; format string
	SUBSP		8
	
	PUSH		R7	; restore return address
	
	PUSH		R1 
	CALLNAT		R1	NATIVE-LOGSTR
	SUBSP		4
	
	RET

	.ENDIF
