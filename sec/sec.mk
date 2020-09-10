
sec: ${LIBDIR}/libsec.a ${BINDIR}/shamir

${LIBDIR}/libsec.a: sec/sec.c sec/sec_shamir.c include/fju/sec.h sec/sec-utils.c
	${CC} -c sec/sec.c sec/sec_shamir.c sec/sec-utils.c ${CFLAGS} 
	${AR} rcs $@ sec.o sec_shamir.o sec-utils.o 

${BINDIR}/shamir: sec/shamir.c ${LIBFJU}
	${CC} -o $@ sec/shamir.c ${CFLAGS} ${LFLAGS} 

LIBRARIES+=sec
PROGRAMS+=shamir
