
sec: ${LIBDIR}/libsec.a ${BINDIR}/fjlic

${LIBDIR}/libsec.a: sec/sec.c sec/sec-shamir.c include/fju/sec.h sec/sec-utils.c
	${CC} -c sec/sec.c sec/sec-shamir.c sec/sec-utils.c ${CFLAGS} 
	${AR} rcs $@ sec.o sec-shamir.o sec-utils.o 

${BINDIR}/fjlic: sec/fjlic.c ${LIBDIR}/libsec.a hostreg freg ftab
	${CC} -o $@ sec/fjlic.c ${CFLAGS} -L${LIBDIR} -lsec -lcrypto -lhostreg -lfreg -lftab -lmmf 

LIBRARIES+=sec
PROGRAMS+=fjlic
