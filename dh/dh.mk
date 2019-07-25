
dh: ${BINDIR}/ecdh ${BINDIR}/dh 

${BINDIR}/ecdh: dh/ecdh.c libsec 
	${CC} -o $@ dh/ecdh.c ${CFLAGS} ${LFLAGS} -lsec -lcrypto 

${BINDIR}/dh: dh/dh.c libsec 
	${CC} -o $@ dh/dh.c ${CFLAGS} ${LFLAGS} -lsec -lcrypto 
