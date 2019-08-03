
ef: ${BINDIR}/ef

fastpbkdf2.o: ef/fastpbkdf2.c ef/fastpbkdf2.h
	cc -c -I/usr/include/sys -std=c99 ef/fastpbkdf2.c

${BINDIR}/ef: ef/ef.h ef/ef_shared.c ef/ef.c fastpbkdf2.o 
	cc -o ${BINDIR}/ef ef/ef.c ef/ef_shared.c fastpbkdf2.o -lcrypto

