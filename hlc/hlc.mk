
hlc: ${BINDIR}/hlc ${LIBDIR}/libhlc.a hlc/hlc-fvm.c 

${LIBDIR}/libhlc.a: hlc/hlc.c include/fju/hlc.h 
	${CC} -c hlc/hlc.c ${CFLAGS} 
	${AR} rcs $@ hlc.o 

${BINDIR}/hlc: hlc/hlc-main.c ${LIBFJU}
	${CC} -o $@ hlc/hlc-main.c ${CFLAGS} ${LFLAGS}

hlc/hlc.fvm: hlc/hlc.pas ${BINDIR}/fju 
	${BINDIR}/fju fvmc -I fvm/stdlib hlc/hlc.pas

hlc/hlc-fvm.c: hlc/hlc.fvm ${BINDIR}/fju 
	${BINDIR}/fju fvmc -o hlc/hlc-fvm.c -C hlc/hlc.fvm 

PROGRAMS+=hlc
LIBRARIES+=hlc
FVMMODULES += hlc/hlc-fvm.c
