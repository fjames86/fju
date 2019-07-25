
.PHONY: librpc 
rpc: ${BINDIR}/rpcd ${BINDIR}/rpcinfo librpc

lubrpc: ${BINDIR}/librpc.a 

${BINDIR}/librpc.a: rpc/rpc.c rpc/rpcd.c rpc/shauth.c 
	${CC} -c $> ${CFLAGS} 
	${AR} rcs $@ rpc.o rpcd.o shauth.o 

${BINDIR}/rpcd: rpc/rpcd-main.c libmmf librpc libsec liblog libraft libhrauth libnls librex
	${CC} -o $@ rpc/rpcd-main.c ${CFLAGS} ${LFLAGS} -lmmf -lrpc -lcrypto -llog -lsec -lraft -lhostreg -lhrauth -lnls -lrex 

${BINDIR}/rpcinfo: rpc/rpcinfo.c librpc libhrauth libhostreg libsec libmmf 
	${CC} -o $@ rpc/rpcinfo.c ${CFLAGS} ${LFLAGS} -lmmf -lrpc -lcrypto -lsec -lhostreg -lhrauth 
