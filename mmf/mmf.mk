
mmf: ${LIBDIR}/libmmf.a

${LIBDIR}/libmmf.a: mmf/mmf.c include/fju/mmf.h 
	${CC} -c mmf/mmf.c ${CFLAGS} 
	${AR} rcs $@ mmf.o

