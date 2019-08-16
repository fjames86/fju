
lht: ${BINDIR}/lht ${LIBDIR}/liblht.a 

${LIBDIR}/liblht.a: lht/lht.c include/fju/lht.h 
	${CC} -c lht/lht.c ${CFLAGS} 
	${AR} rcs $@ lht.o

${BINDIR}/lht: lht/lht-main.c ${LIBFJU}
	${CC} -o $@ lht/lht-main.c ${CFLAGS} ${LFLAGS} 

LIBRARIES+=lht
