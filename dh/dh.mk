
dh: ${BINDIR}/dh 

${BINDIR}/dh: dh/dh.c ${LIBFJU}
	${CC} -o $@ dh/dh.c ${CFLAGS} ${LFLAGS}

