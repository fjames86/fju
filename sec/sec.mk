
sec: ${LIBDIR}/libsec.a ${BINDIR}/shamir

${LIBDIR}/libsec.a: sec/sec.c sec/sec_shamir.c include/fju/sec.h 
	${CC} -c sec/sec.c sec/sec_shamir.c ${CFLAGS} 
	${AR} rcs $@ sec.o sec_shamir.o 

${BINDIR}/shamir: sec/shamir.c ${LIBFJU}
	${CC} -o $@ sec/shamir.c ${CFLAGS} ${LFLAGS} 

LIBRARIES+=sec
PROGRAMS+=shamir
