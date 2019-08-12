
ftab: ${LIBDIR}/libftab.a ${BINDIR}/ftab 

${LIBDIR}/libftab.a: ftab/ftab.c include/fju/ftab.h 
	${CC} -c ${CFLAGS} ftab/ftab.c 
	${AR} rcs $@ ftab.o


${BINDIR}/ftab: ftab/ftab-main.c ${LIBDIR}/libmmf.a ${LIBDIR}/libftab.a
	${CC} -o $@ ftab/ftab-main.c ${CFLAGS} ${LFLAGS} -lmmf -lftab -lsec -lcrypto 
