
ftab: ${LIBDIR}/libftab.a ${BINDIR}/ftab ${BINDIR}/fdtab

${LIBDIR}/libftab.a: ftab/ftab.c include/fju/ftab.h ftab/fdtab.c include/fju/fdtab.h 
	${CC} -c ${CFLAGS} ftab/ftab.c ftab/fdtab.c 
	${AR} rcs $@ ftab.o fdtab.o

${BINDIR}/ftab: ftab/ftab-main.c ${LIBFJU}
	${CC} -o $@ ftab/ftab-main.c ${CFLAGS} ${LFLAGS}

${BINDIR}/fdtab: ftab/fdtab-main.c ${LIBFJU}
	${CC} -o $@ ftab/fdtab-main.c ${CFLAGS} ${LFLAGS}

PROGRAMS+=ftab
PROGRAMS+=fdtab
LIBRARIES+=ftab

