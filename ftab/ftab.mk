
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

${BINDIR}/ftab: ftab/ftab-main.c ${LIBDIR}/libmmf.a ${LIBDIR}/libftab.a
	${CC} -o $@ ftab/ftab-main.c ${CFLAGS} ${LFLAGS} -lmmf -lftab -lsec -lcrypto 

${BINDIR}/fdtab: ftab/fdtab-main.c ${LIBDIR}/libmmf.a ${LIBDIR}/libfdtab.a ${LIBDIR}/libftab.a
	${CC} -o $@ ftab/fdtab-main.c ${CFLAGS} ${LFLAGS} -lfdtab -lmmf -lftab -lsec -lcrypto 

${BINDIR}/freg: ftab/freg-main.c ${LIBDIR}/libmmf.a ${LIBDIR}/libfdtab.a ${LIBDIR}/libftab.a ${LIBDIR}/libfreg.a
	${CC} -o $@ ftab/freg-main.c ${CFLAGS} ${LFLAGS} -lfdtab -lmmf -lftab -lsec -lcrypto -lfreg 
