
nls: ${BINDIR}/libnls.a ${BINDIR}/nls

${BINDIR}/libnls.a: nls/nls.c nls/nls-rpc.c 
	${CC} -c $> ${CFLAGS} 
	${AR} rcs $@ nls.o nls-rpc.o 

${BINDIR}/nls: nls/nls-main.c mmf sec log ${BINDIR}/libnls.a 
	${CC} -o $@ nls/nls-main.c ${CFLAGS} ${LFLAGS} -lmmf -lcrypto -llog -lsec -lnls 
