
raft: ${LIBDIR}/libraft.a 

${LIBDIR}/libraft.a: raft/raft.c include/fju/raft.h raft/raft-example.c 
	${CC} -c raft/raft.c raft/raft-example.c ${CFLAGS} 
	${AR} rcs $@ raft.o raft-example.o 

LIBRARIES+=raft
