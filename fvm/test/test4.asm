
	.MODULE		test4

	.INCLUDE	"native.asm"

	.TEXT		keyname "test4"
	.TEXT		keyval	0x1234
MAIN:
	;; try calling freg-get(name,flags,buf,size)
	PUSH		keyname
	PUSH		FREG-TYPE-UINT32
	PUSH		keyval
	PUSH		4
	CALLNAT		R0		NATIVE-FREG-GET
	SUBSP		16 

	LDI		R0		keyval
	LD		R1		R0
	ADD		R1		1
	ST		R0		R1
	
	;; call freg-put(name,flags,buf,size)
	PUSH		keyname
	PUSH		FREG-TYPE-UINT32
	PUSH		keyval
	PUSH		4
	CALLNAT		R0		NATIVE-FREG-PUT
	SUBSP		16
	
	RET

	.EXPORT		MAIN
	
