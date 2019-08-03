
mmf: ${LIBDIR}/libmmf.a

${LIBDIR}/libmmf.a: mmf/mmf.c include/mmf.h 
	${CC} -c $> ${CFLAGS} 
	${AR} rcs $@ mmf.o

