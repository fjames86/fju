
raft: ${BINDIR}/raft ${BINDIR}/libraft.a ${BINDIR}/librex.a 

${BINDIR}/libraft.a: raft/raft.c raft/raft-rpc.c include/raft.h 
	${CC} -c raft/raft.c raft/raft-rpc.c ${CFLAGS} 
	${AR} rcs $@ raft.o raft-rpc.o 

${BINDIR}/librex.a: raft/rex.c include/rex.h 
	${CC} -c raft/rex.c ${CFLAGS} 
	${AR} rcs $@ rex.o 

raft_deps += ${BINDIR}/libmmf.a
raft_deps += ${BINDIR}/liblog.a
raft_deps += ${BINDIR}/libsec.a
raft_deps += ${BINDIR}/libhostreg.a
raft_deps += ${BINDIR}/libhrauth.a
raft_deps += ${BINDIR}/librpc.a
raft_deps += ${BINDIR}/libraft.a

${BINDIR}/raft: raft/raft-main.c ${raft_deps}
	${CC} -o $@ raft/raft-main.c ${CFLAGS} ${LFLAGS} -lmmf -lcrypto -llog -lsec -lrpc -lraft -lhostreg -lhrauth
