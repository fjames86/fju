
;;; This file contains the proc ids of the native functions

	.CONST		MODULE-NATIVE		0
	.CONST		NATIVE-NOP		0
	.CONST		NATIVE-PUTS		1
	.CONST		NATIVE-RAND		2
	.CONST 		NATIVE-NOW 		3
	.CONST		NATIVE-LOGSTR		4
	.CONST		NATIVE-READ		5
	.CONST		NATIVE-WRITE		6
	.CONST		NATIVE-GETPROGID	7
	.CONST		NATIVE-WRITELOG		8
	.CONST		NATIVE-READLOG		9
	.CONST		NATIVE-FREGGET-U32	10
	.CONST		NATIVE-FREGPUT-U32	11
	.CONST		NATIVE-FREGGET-BUF	12
	.CONST		NATIVE-FREGPUT-BUF	13
	.CONST		NATIVE-SPRINTF		14
	.CONST		NATIVE-YIELD		15 	

	;; freg constants
	.CONST		FREG-TYPE-OPAQUE	0x0000
	.CONST		FREG-TYPE-UINT32	0x0001
	.CONST		FREG-TYPE-UINT64	0x0002
	.CONST		FREG-TYPE-STRING	0x0003
	.CONST		FREG-TYPE-KEY		0x0004
	
	
	.CONST		YIELD-FORK		1 ; process is forked and will continue at the specified timeout. execution continues
	
