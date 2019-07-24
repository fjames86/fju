
dh: ${BINDIR}/ecdh ${BINDIR}/dh 

${BINDIR}/ecdh: dh/ecdh.c ${BINDIR}/libsec.a
	${CC} -o $@ dh/ecdh.c ${CFLAGS} ${LFLAGS} -lsec -lcrypto 

${BINDIR}/dh: dh/dh.c ${BINDIR}/libsec.a
	${CC} -o $@ dh/dh.c ${CFLAGS} ${LFLAGS} -lsec -lcrypto 
