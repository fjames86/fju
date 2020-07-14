
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

	.UNDEF	bob

	.IFDEF	bob
	.TEXT	mystr3	"bob"
	.ENDIF
	
	.CONST	INCLUDE-LOGF
	.INCLUDE "stdlib.asm"
	.UNDEF INCLUDE-LOGF

	.INCLUDE	"logf.asm" %LOGF	
	.CALL	LOGF	R0 R1 R2
	
	.TEXT	xxxxxxxx "xxxxxxxxxend" 
