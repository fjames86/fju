
sec: ${BINDIR}/libsec.a 

${BINDIR}/libsec.a: sec/sec.c include/sec.h 
	${CC} -c $> ${CFLAGS} 
	${AR} rcs $@ sec.o
