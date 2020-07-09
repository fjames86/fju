
	.MODULE		test4

	.INCLUDE	"native.asm"

	.TEXT		keynameu32 	"test4-u32"
	.TEXT		keynamestr 	"test4-str"
	.TEXT		teststr 	"test4 hello world!"
	.TEXT		testfmt		"test format \"%s\"\n"
	
MAIN:
	;; call fregget-u32(name)
	PUSH		keynameu32
	CALLNAT		R0		NATIVE-FREGGET-U32
	SUBSP		4

	;; increment whatever value got returned 
	ADD		R0		1
	
	;; call fregput-u32(name,val)
	PUSH		R0		
	PUSH		keynameu32
	CALLNAT		R0		NATIVE-FREGPUT-U32
	SUBSP		8

	;; call fregput-buf(name,flags,buf,len)
	LD		R0		teststr	; get string length
	PUSH		R0			; push len
	LDI		R0		teststr
	ADD		R0		4
	PUSH		R0	; push buf
	PUSH		FREG-TYPE-STRING ; push flags
	PUSH		keynamestr
	CALLNAT		R0		NATIVE-FREGPUT-BUF
	SUBSP		16

	LEASP		R1		0 ; get some space on stack
	MOV		R2		R1
	ADD		R2		4 ; reserve length prefix 
	ADDSP		68
	PUSH		64 	; push len
	PUSH		R2 	; push buf
	PUSH		FREG-TYPE-STRING
	PUSH		keynamestr
	CALLNAT		R0		NATIVE-FREGGET-BUF
	SUBSP		16
	ST		R1		R0 ; save length
	
	PUSH		R1
	CALLNAT		R0		NATIVE-PUTS
	SUBSP		4

;;;  --------- test format

	PUSH		teststr	; arg0
	PUSH		testfmt	;fmt
	PUSH		68	; len
	PUSH		R1	; buf
	;;  sprintf(buf,len,fmt,...)
	CALLNAT		R0		NATIVE-SPRINTF
	SUBSP		16
	
	PUSH		R1
	CALLNAT		R0		NATIVE-PUTS
	SUBSP		4 
	
	
	SUBSP		68 	; free string

	

	
	RET

	.EXPORT		MAIN
	
