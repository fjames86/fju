
nls: ${BINDIR}/nls ${LIBDIR}/libnls.a 

${LIBDIR}/libnls.a: nls/nls.c nls/nls-rpc.c include/fju/nls.h 
	${CC} -c nls/nls.c nls/nls-rpc.c ${CFLAGS} 
	${AR} rcs $@ nls.o nls-rpc.o 

nls_deps += ${LIBDIR}/libmmf.a
nls_deps += ${LIBDIR}/liblog.a
nls_deps += ${LIBDIR}/libsec.a
nls_deps += ${LIBDIR}/libnls.a
nls_deps += ${LIBDIR}/libhostreg.a

${BINDIR}/nls: nls/nls-main.c ${nls_deps}
	${CC} -o $@ nls/nls-main.c ${CFLAGS} ${LFLAGS} -lmmf -lcrypto -llog -lsec -lnls -lhostreg

LIBRARIES+=nls
