
ef: ${BINDIR}/ef

${BINDIR}/ef: ef/ef.h ef/ef_shared.c ef/ef.c ef/fastpbkdf2.c
	cc -c -I/usr/include/sys -std=c99 ef/fastpbkdf2.c
	cc -o ${BINDIR}/ef ef/ef.c ef/ef_shared.c fastpbkdf2.o -lcrypto

PROGRAMS+=ef

