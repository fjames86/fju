
raft: ${BINDIR}/raft ${LIBDIR}/libraft.a ${LIBDIR}/librex.a 

${LIBDIR}/libraft.a: raft/raft.c raft/raft-rpc.c include/fju/raft.h 
	${CC} -c raft/raft.c raft/raft-rpc.c ${CFLAGS} 
	${AR} rcs $@ raft.o raft-rpc.o 

${LIBDIR}/librex.a: raft/rex.c include/fju/rex.h 
	${CC} -c raft/rex.c ${CFLAGS} 
	${AR} rcs $@ rex.o 

${BINDIR}/raft: raft/raft-main.c ${LIBFJU}
	${CC} -o $@ raft/raft-main.c ${CFLAGS} ${LFLAGS} -lfju -lcrypto

LIBRARIES+=raft
LIBRARIES+=rex

