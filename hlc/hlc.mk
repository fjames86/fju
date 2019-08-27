
hlc: ${BINDIR}/hlc ${LIBDIR}/libhlc.a 

${LIBDIR}/libhlc.a: hlc/hlc.c include/fju/hlc.h 
	${CC} -c hlc/hlc.c ${CFLAGS} 
	${AR} rcs $@ hlc.o 

${BINDIR}/hlc: hlc/hlc-main.c ${LIBFJU}
	${CC} -o $@ hlc/hlc-main.c ${CFLAGS} ${LFLAGS}

PROGRAMS+=hlc
LIBRARIES+=hlc
