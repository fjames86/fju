
libmmf.a: mmf.c mmf.h
	gcc -c mmf.c -o mmf.o 
	ar rcs libmmf.a mmf.o 

test: test.c 
	gcc -o test test.c -L. -lmmf 
