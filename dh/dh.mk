
dh: ${BINDIR}/ecdh ${BINDIR}/dh 

${BINDIR}/ecdh: dh/ecdh.c ${LIBFJU}
	${CC} -o $@ dh/ecdh.c ${CFLAGS} ${LFLAGS} 

${BINDIR}/dh: dh/dh.c ${LIBFJU}
	${CC} -o $@ dh/dh.c ${CFLAGS} ${LFLAGS}

PROGRAMS+=ecdh
