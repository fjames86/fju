

rpc: ${BINDIR}/fjud ${LIBDIR}/librpc.a ${BINDIR}/rpclt ${BINDIR}/xdru

${LIBDIR}/librpc.a: rpc/rpc.c rpc/rpcd.c rpc/shauth.c include/fju/rpc.h include/fju/rpcd.h include/fju/shauth.h rpc/rpc-private.h 
	${CC} -c rpc/rpc.c rpc/rpcd.c rpc/shauth.c ${CFLAGS} 
	${AR} rcs $@ rpc.o rpcd.o shauth.o 

${BINDIR}/fjud: rpc/fjud-main.c rpc/cmdprog.c rpc/rpc-private.h ${LIBFJU}
	${CC} -o $@ rpc/fjud-main.c rpc/cmdprog.c ${CFLAGS} ${LFLAGS} 

${BINDIR}/rpclt: rpc/rpclt.c rpc/rpc-private.h ${LIBFJU}
	${CC} -o $@ rpc/rpclt.c ${CFLAGS} ${LFLAGS} 

${BINDIR}/xdru: rpc/xdru.c ${LIBFJU}
	${CC} -o $@ rpc/xdru.c ${CFLAGS} ${LFLAGS} 

PROGRAMS+=fjud
PROGRAMS+=rpclt
LIBRARIES+=rpc
PROGRAMS+=xdru


