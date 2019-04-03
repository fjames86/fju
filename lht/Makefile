
.PHONY: all clean

all: liblht.a lht

clean:
	rm liblht.a lht lht.o

liblht.a: lht.c lht.h
	cc -c -g lht.c -I../log -I../mmf 
	ar rcs liblht.a lht.o

lht: lht-main.c liblht.a
	cc -o lht -g lht-main.c -L. -llht -L../log -llog -I../log -I../mmf -L../mmf -lmmf

