
all: ef

fastpbkdf2.o: fastpbkdf2.c fastpbkdf2.h
	cc -c -I/usr/include/sys -std=c99 fastpbkdf2.c

ef: ef.h ef_shared.c ef.c fastpbkdf2.o 
	cc -o ef ef.c ef_shared.c fastpbkdf2.o -lcrypto

.PHONY: clean 
clean:
	rm ef fastpbkdf2.o 

install: ef 
	cp ef /usr/local/bin/ef

tar: 
	cd .. && tar -czvf ef/ef.tar.gz ef/ef.c ef/ef.h ef/ef_shared.c ef/fastpbkdf2.c ef/fastpbkdf2.h ef/Makefile
