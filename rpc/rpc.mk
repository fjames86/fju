

rpc: ${BINDIR}/fjud ${LIBDIR}/librpc.a ${BINDIR}/rpclt ${LIBDIR}/libsvctest.so

${LIBDIR}/librpc.a: rpc/rpc.c rpc/rpcd.c rpc/shauth.c include/fju/rpc.h include/fju/rpcd.h include/fju/shauth.h rpc/rpc-private.h 
	${CC} -c rpc/rpc.c rpc/rpcd.c rpc/shauth.c ${CFLAGS} 
	${AR} rcs $@ rpc.o rpcd.o shauth.o 

${BINDIR}/fjud: rpc/fjud-main.c rpc/rpc-private.h ${LIBFJU}
	${CC} -o $@ rpc/fjud-main.c ${CFLAGS} ${LFLAGS} 

${BINDIR}/rpclt: rpc/rpclt.c rpc/rpc-private.h ${LIBFJU}
	${CC} -o $@ rpc/rpclt.c ${CFLAGS} ${LFLAGS} 

${LIBDIR}/libsvctest.so: rpc/svctest.c ${LIBFJU}
	cc -shared -o $@ ${CFLAGS} rpc/svctest.c -L${LIBDIR} -lfju

PROGRAMS+=fjud
PROGRAMS+=rpclt
LIBRARIES+=rpc
