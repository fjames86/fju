
.PHONY: libnls

nls: ${BINDIR}/nls libnls

libnls: ${BINDIR}/libnls.a 

${BINDIR}/libnls.a: nls/nls.c nls/nls-rpc.c librpc liblog 
	${CC} -c nls/nls.c nls/nls-rpc.c ${CFLAGS} 
	${AR} rcs $@ nls.o nls-rpc.o 

${BINDIR}/nls: nls/nls-main.c libmmf libsec liblog libnls 
	${CC} -o $@ nls/nls-main.c ${CFLAGS} ${LFLAGS} -lmmf -lcrypto -llog -lsec -lnls 
