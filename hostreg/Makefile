
.PHONY: all clean

all: hostreg libhostreg.a

clean:
	rm hostreg libhostreg.a hostreg.o

libhostreg.a: hostreg.c hostreg.h
	gcc -c hostreg.c -I../mmf -I../sec 
	ar rcs libhostreg.a hostreg.o

hostreg: hostreg-main.c libhostreg.a
	gcc -o hostreg -g hostreg-main.c -L. -lhostreg -L../mmf -lmmf -I../mmf -I../sec -lws2_32
