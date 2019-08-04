
sec: ${LIBDIR}/libsec.a 

${LIBDIR}/libsec.a: sec/sec.c include/fju/sec.h 
	${CC} -c sec/sec.c ${CFLAGS} 
	${AR} rcs $@ sec.o
