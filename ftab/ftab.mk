
ftab: ${LIBDIR}/libftab.a ${BINDIR}/ftab ${LIBDIR}/libfdtab.a ${BINDIR}/fdtab ${LIBDIR}/libfreg.a ${BINDIR}/freg 

${LIBDIR}/libftab.a: ftab/ftab.c include/fju/ftab.h 
	${CC} -c ${CFLAGS} ftab/ftab.c 
	${AR} rcs $@ ftab.o

${LIBDIR}/libfdtab.a: ftab/fdtab.c include/fju/fdtab.h 
	${CC} -c ${CFLAGS} ftab/fdtab.c 
	${AR} rcs $@ fdtab.o

${LIBDIR}/libfreg.a: ftab/freg.c include/fju/freg.h 
	${CC} -c ${CFLAGS} ftab/freg.c 
	${AR} rcs $@ freg.o

${BINDIR}/ftab: ftab/ftab-main.c ${LIBFJU}
	${CC} -o $@ ftab/ftab-main.c ${CFLAGS} ${LFLAGS}

${BINDIR}/fdtab: ftab/fdtab-main.c ${LIBFJU}
	${CC} -o $@ ftab/fdtab-main.c ${CFLAGS} ${LFLAGS}

${BINDIR}/freg: ftab/freg-main.c ${LIBFJU}
	${CC} -o $@ ftab/freg-main.c ${CFLAGS} ${LFLAGS}


LIBRARIES+=ftab
LIBRARIES+=fdtab
LIBRARIES+=freg
