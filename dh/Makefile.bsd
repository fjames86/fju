
.PHONY: all clean
all: dh ecdh 

clean:
	rm dh ecdh 

dh: dh.c
	cc -o dh dh.c -lcrypto

ecdh: ecdh.c 
	cc -o ecdh -g ecdh.c -lcrypto -I../sec -L../sec -lsec 
