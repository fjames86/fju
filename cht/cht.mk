
cht: ${BINDIR}/cht ${LIBDIR}/libcht.a 

${LIBDIR}/libcht.a: cht/cht.c include/fju/cht.h cht/cht-rsync.c 
	${CC} -c cht/cht.c cht/cht-rsync.c ${CFLAGS} 
	${AR} rcs $@ cht.o cht-rsync.o

${BINDIR}/cht: cht/cht-main.c ${LIBFJU}
	${CC} -o $@ cht/cht-main.c ${CFLAGS} ${LFLAGS} 

PROGRAMS+=cht
LIBRARIES+=cht
