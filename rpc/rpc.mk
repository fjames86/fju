

rpc: ${BINDIR}/rpcd ${BINDIR}/rpcinfo ${BINDIR}/librpc.a 

${BINDIR}/librpc.a: rpc/rpc.c rpc/rpcd.c rpc/shauth.c include/rpc.h include/rpcd.h include/shauth.h 
	${CC} -c rpc/rpc.c rpc/rpcd.c rpc/shauth.c ${CFLAGS} 
	${AR} rcs $@ rpc.o rpcd.o shauth.o 

rpc_deps += ${BINDIR}/libmmf.a
rpc_deps += ${BINDIR}/librpc.a
rpc_deps += ${BINDIR}/libsec.a
rpc_deps += ${BINDIR}/libhostreg.a
rpc_deps += ${BINDIR}/liblog.a
rpc_deps += ${BINDIR}/libhrauth.a
rpc_deps += ${BINDIR}/libraft.a
rpc_deps += ${BINDIR}/librex.a
rpc_deps += ${BINDIR}/libnls.a

${BINDIR}/rpcd: rpc/rpcd-main.c ${rpc_deps}
	${CC} -o $@ rpc/rpcd-main.c ${CFLAGS} ${LFLAGS} -lmmf -lrpc -lcrypto -llog -lsec -lraft -lhostreg -lhrauth -lnls -lrex 

${BINDIR}/rpcinfo: rpc/rpcinfo.c ${rpc_deps}
	${CC} -o $@ rpc/rpcinfo.c ${CFLAGS} ${LFLAGS} -lmmf -lrpc -lcrypto -lsec -lhostreg -lhrauth 
