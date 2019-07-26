
sec: ${BINDIR}/libsec.a 

${BINDIR}/libsec.a: sec/sec.c 
	${CC} -c $> ${CFLAGS} 
	${AR} rcs $@ sec.o
