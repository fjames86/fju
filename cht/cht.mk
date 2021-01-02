
cht: ${LIBDIR}/libcht.a 

${LIBDIR}/libcht.a: cht/cht.c include/fju/cht.h 
	${CC} -c cht/cht.c ${CFLAGS} 
	${AR} rcs $@ cht.o 

LIBRARIES+=cht
