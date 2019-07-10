
.PHONY: all clean

all: nls libnls.a

clean:
	rm -f nls libnls.a nls.o

libnls.a: nls.c nls.h
	cc -c nls.c -I../mmf -I../sec -I../rpc -I../log -I../hostreg 
	ar rcs libnls.a nls.o

nls: nls-main.c libnls.a
	cc -o nls nls-main.c -L. -lnls -I../mmf -L../mmf -lmmf -I../sec -L../sec -lsec -lcrypto -I../rpc -L../rpc -lrpc -I../log -L../log -llog -L../hostreg -lhostreg



