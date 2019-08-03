

rpc: ${BINDIR}/rpcd ${BINDIR}/rpcinfo ${LIBDIR}/librpc.a ${BINDIR}/rpclt

${LIBDIR}/librpc.a: rpc/rpc.c rpc/rpcd.c rpc/shauth.c include/rpc.h include/rpcd.h include/shauth.h 
	${CC} -c rpc/rpc.c rpc/rpcd.c rpc/shauth.c ${CFLAGS} 
	${AR} rcs $@ rpc.o rpcd.o shauth.o 

rpc_deps += ${LIBDIR}/libmmf.a
rpc_deps += ${LIBDIR}/librpc.a
rpc_deps += ${LIBDIR}/libsec.a
rpc_deps += ${LIBDIR}/libhostreg.a
rpc_deps += ${LIBDIR}/liblog.a
rpc_deps += ${LIBDIR}/libhrauth.a
rpc_deps += ${LIBDIR}/libraft.a
rpc_deps += ${LIBDIR}/librex.a
rpc_deps += ${LIBDIR}/libnls.a

${BINDIR}/rpcd: rpc/rpcd-main.c ${rpc_deps}
	${CC} -o $@ rpc/rpcd-main.c ${CFLAGS} ${LFLAGS} -lmmf -lrpc -lcrypto -llog -lsec -lraft -lhostreg -lhrauth -lnls -lrex 

${BINDIR}/rpcinfo: rpc/rpcinfo.c ${rpc_deps}
	${CC} -o $@ rpc/rpcinfo.c ${CFLAGS} ${LFLAGS} -lmmf -lrpc -lcrypto -lsec -lhostreg -lhrauth 

${BINDIR}/rpclt: rpc/rpclt.c ${rpc_deps}
	${CC} -o $@ rpc/rpclt.c ${CFLAGS} ${LFLAGS} -lmmf -lrpc -lcrypto -lsec -lhostreg -lhrauth -lraft 
