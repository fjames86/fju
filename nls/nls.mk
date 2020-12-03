
nls: ${BINDIR}/nls ${LIBDIR}/libnls.a ${BINDIR}/nls.fvm 

${LIBDIR}/libnls.a: nls/nls.c nls/nls-rpc.c include/fju/nls.h 
	${CC} -c nls/nls.c nls/nls-rpc.c ${CFLAGS} 
	${AR} rcs $@ nls.o nls-rpc.o 

${BINDIR}/nls: nls/nls-main.c ${LIBFJU}
	${CC} -o $@ nls/nls-main.c ${CFLAGS} ${LFLAGS} 

${BINDIR}/nls.fvm: ${BINDIR}/fvmc nls/nls.pas
	${BINDIR}/fvmc -o $@ fvm/stdlib/native.pas nls/nls.pas
	rm nls/nls.asm

PROGRAMS+=nls
LIBRARIES+=nls
