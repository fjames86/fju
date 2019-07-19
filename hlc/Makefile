
.PHONY: all clean

all: libhlc.a hlc

clean:
	rm libhlc.a hlc hlc.o

libhlc.a: hlc.c hlc.h
	cc -c hlc.c -I../log -I../mmf -I../sec 
	ar rcs libhlc.a hlc.o

hlc: hlc-main.c libhlc.a
	cc -o hlc -g hlc-main.c -L. -lhlc -I../mmf -L../mmf -lmmf -I../log -L../log -llog -I../sec -L../sec -lsec -lcrypto

