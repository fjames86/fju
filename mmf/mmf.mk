
mmf: libmmf 

libmmf: ${BINDIR}/libmmf.a

${BINDIR}/libmmf.a: mmf/mmf.c
	${CC} -c $> ${CFLAGS} 
	${AR} rcs $@ mmf.o

