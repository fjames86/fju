
rpc: ${BINDIR}/librpc.a ${BINDIR}/rpcd ${BINDIR}/rpcinfo

${BINDIR}/librpc.a: rpc/rpc.c rpc/rpcd.c rpc/shauth.c 
	${CC} -c $> ${CFLAGS} 
	${AR} rcs $@ rpc.o rpcd.o shauth.o 

${BINDIR}/rpcd: rpc/rpcd-main.c mmf ${BINDIR}/librpc.a sec log ${BINDIR}/libraft.a ${BINDIR}/libhrauth.a ${BINDIR}/libnls.a ${BINDIR}/librex.a 
	${CC} -o $@ rpc/rpcd-main.c ${CFLAGS} ${LFLAGS} -lmmf -lrpc -lcrypto -llog -lsec -lraft -lhostreg -lhrauth -lnls -lrex 

${BINDIR}/rpcinfo: rpc/rpcinfo.c ${BINDIR}/librpc.a
	${CC} -o $@ rpc/rpcinfo.c ${CFLAGS} ${LFLAGS} -lmmf -lrpc -lcrypto -lsec -lhostreg -lhrauth 
