
sec: ${LIBDIR}/libsec.a ${BINDIR}/shamir ${BINDIR}/fjlic

${LIBDIR}/libsec.a: sec/sec.c sec/sec-shamir.c include/fju/sec.h sec/sec-utils.c
	${CC} -c sec/sec.c sec/sec-shamir.c sec/sec-utils.c ${CFLAGS} 
	${AR} rcs $@ sec.o sec-shamir.o sec-utils.o 

${BINDIR}/shamir: sec/shamir.c ${LIBFJU}
	${CC} -o $@ sec/shamir.c ${CFLAGS} ${LFLAGS}

${BINDIR}/fjlic: sec/fjlic.c ${LIBFJU}
	${CC} -o $@ sec/fjlic.c ${CFLAGS} ${LFLAGS} 

LIBRARIES+=sec
PROGRAMS+=shamir
PROGRAMS+=fjlic
