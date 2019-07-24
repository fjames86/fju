
raft: ${BINDIR}/libraft.a ${BINDIR}/raft ${BINDIR}/librex.a 

${BINDIR}/libraft.a: raft/raft.c raft/raft-rpc.c 
	${CC} -c $> ${CFLAGS} 
	${AR} rcs $@ raft.o raft-rpc.o 

${BINDIR}/librex.a: raft/rex.c
	${CC} -c $> ${CFLAGS} 
	${AR} rcs $@ rex.o 

${BINDIR}/raft: raft/raft-main.c mmf sec log ${BINDIR}/librpc.a ${BINDIR}/libraft.a ${BINDIR}/libhostreg.a 
	${CC} -o $@ raft/raft-main.c ${CFLAGS} ${LFLAGS} -lmmf -lcrypto -llog -lsec -lrpc -lraft -lhostreg
