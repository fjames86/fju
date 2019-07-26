
mmf: libmmf 

libmmf: ${BINDIR}/libmmf.a

${BINDIR}/libmmf.a: mmf/mmf.c include/mmf.h 
	${CC} -c $> ${CFLAGS} 
	${AR} rcs $@ mmf.o

