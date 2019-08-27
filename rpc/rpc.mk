

rpc: ${BINDIR}/rpcd ${BINDIR}/rpcinfo ${LIBDIR}/librpc.a ${BINDIR}/rpclt ${LIBDIR}/libsvctest.so

${LIBDIR}/librpc.a: rpc/rpc.c rpc/rpcd.c rpc/shauth.c include/fju/rpc.h include/fju/rpcd.h include/fju/shauth.h rpc/rpc-private.h 
	${CC} -c rpc/rpc.c rpc/rpcd.c rpc/shauth.c ${CFLAGS} 
	${AR} rcs $@ rpc.o rpcd.o shauth.o 

${BINDIR}/rpcd: rpc/rpcd-main.c rpc/rpc-private.h ${LIBFJU}
	${CC} -o $@ rpc/rpcd-main.c ${CFLAGS} ${LFLAGS} 

${BINDIR}/rpcinfo: rpc/rpcinfo.c rpc/rpc-private.h ${LIBFJU}
	${CC} -o $@ rpc/rpcinfo.c ${CFLAGS} ${LFLAGS} 

${BINDIR}/rpclt: rpc/rpclt.c rpc/rpc-private.h ${LIBFJU}
	${CC} -o $@ rpc/rpclt.c ${CFLAGS} ${LFLAGS} 

${LIBDIR}/libsvctest.so: rpc/svctest.c ${LIBFJU}
	cc -shared -o $@ ${CFLAGS} rpc/svctest.c -L${LIBDIR} -lfju

PROGRAMS+=rpcd
PROGRAMS+=rpclt
LIBRARIES+=rpc
