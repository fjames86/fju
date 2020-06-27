
	.MODULE		test
	.PROGRAM	999		1

	.TEXT		hellomsg	"Hello, World!\n"
	
hello-world:	
	PUSH		hellomsg		      ; native/puts requires address of string on stack 
	CALLNAT		R1		NATIVE-PUTS
	RET

echo-string:
	LDI		R3		0
	SUB		R2		R0
	LEASP		R1		R2 ; get address of string on stack
	PUSH		R1
	CALLNAT		R1		NATIVE-PUTS
	RET
	
	.EXPORT		hello-world
	.EXPORT		echo-string
	
