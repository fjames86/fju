
	.MODULE		test
	.PROGRAM	999		1

	.TEXT		hellomsg	"Hello, World!\n"
	.TEXT		thisaddr	$
	
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
echo-string-end:	
	
	.EXPORT		hello-world     PROC 	echo-string 
	.EXPORT		echo-string     PROC	echo-string-end
	.EXPORT		hellomsg 	STRING
	
