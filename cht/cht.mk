
cht: ${BINDIR}/cht ${LIBDIR}/libcht.a 

${LIBDIR}/libcht.a: cht/cht.c include/fju/cht.h 
	${CC} -c cht/cht.c ${CFLAGS} 
	${AR} rcs $@ cht.o

${BINDIR}/cht: cht/cht-main.c ${LIBFJU}
	${CC} -o $@ cht/cht-main.c ${CFLAGS} ${LFLAGS} 

PROGRAMS+=cht
LIBRARIES+=cht
