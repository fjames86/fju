
raft: ${BINDIR}/raft ${LIBDIR}/libraft.a ${LIBDIR}/librex.a 

${LIBDIR}/libraft.a: raft/raft.c raft/raft-rpc.c include/raft.h 
	${CC} -c raft/raft.c raft/raft-rpc.c ${CFLAGS} 
	${AR} rcs $@ raft.o raft-rpc.o 

${LIBDIR}/librex.a: raft/rex.c include/rex.h 
	${CC} -c raft/rex.c ${CFLAGS} 
	${AR} rcs $@ rex.o 

raft_deps += ${LIBDIR}/libmmf.a
raft_deps += ${LIBDIR}/liblog.a
raft_deps += ${LIBDIR}/libsec.a
raft_deps += ${LIBDIR}/libhostreg.a
raft_deps += ${LIBDIR}/libhrauth.a
raft_deps += ${LIBDIR}/librpc.a
raft_deps += ${LIBDIR}/libraft.a

${BINDIR}/raft: raft/raft-main.c ${raft_deps}
	${CC} -o $@ raft/raft-main.c ${CFLAGS} ${LFLAGS} -lmmf -lcrypto -llog -lsec -lrpc -lraft -lhostreg -lhrauth
