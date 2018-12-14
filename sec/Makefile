
.PHONY: all clean

all: libsec.a sectest 

libsec.a: sec.c sec.h 
	gcc -c sec.c 
	ar rcs libsec.a sec.o 

sectest: sectest.c libsec.a 
	gcc -o sectest sectest.c -L. -lsec -lcrypto

clean: 
	rm libsec.a sec.o 

