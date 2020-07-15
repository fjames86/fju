
;;; This file is for testing ifdef/endif macros and .call macro
	
	.IFDEF	bob
	.TEXT	mystr1 "bob"
	.ENDIF

	.IFNDEF	bob
	.TEXT	mystr2 "not bob"
	.ENDIF

	.CONST	bob
	
	.IFDEF	bob
	.TEXT	mystr1	"bob"
	.ENDIF

	.INCLUDE	"logf.asm" %LOGF	
	.CALL	LOGF	R0 R1 R2

	.CALLNAT NATIVE-PUTS	endstr
	.CALLNAT NATIVE-PUTS	endstr	
	RET
	
	.TEXT	endstr	"endstr"
	RET
	ADD	R0	1

	JMP	$-8
	RET

