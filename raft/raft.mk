
.PHONY: libraft librex 

raft: ${BINDIR}/raft libraft librex 

libraft: ${BINDIR}/libraft.a

librex: ${BINDIR}/librex.a

${BINDIR}/libraft.a: raft/raft.c raft/raft-rpc.c 
	${CC} -c $> ${CFLAGS} 
	${AR} rcs $@ raft.o raft-rpc.o 

${BINDIR}/librex.a: raft/rex.c
	${CC} -c $> ${CFLAGS} 
	${AR} rcs $@ rex.o 

${BINDIR}/raft: raft/raft-main.c libmmf libsec liblog librpc libraft libhostreg librex libhrauth
	${CC} -o $@ raft/raft-main.c ${CFLAGS} ${LFLAGS} -lmmf -lcrypto -llog -lsec -lrpc -lraft -lhostreg -lhrauth
