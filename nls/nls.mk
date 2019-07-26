
nls: ${BINDIR}/nls ${BINDIR}/libnls.a 

${BINDIR}/libnls.a: nls/nls.c nls/nls-rpc.c 
	${CC} -c nls/nls.c nls/nls-rpc.c ${CFLAGS} 
	${AR} rcs $@ nls.o nls-rpc.o 

nls_deps += ${BINDIR}/libmmf.a
nls_deps += ${BINDIR}/liblog.a
nls_deps += ${BINDIR}/libsec.a
nls_deps += ${BINDIR}/libnls.a

${BINDIR}/nls: nls/nls-main.c ${nls_deps}
	${CC} -o $@ nls/nls-main.c ${CFLAGS} ${LFLAGS} -lmmf -lcrypto -llog -lsec -lnls 
