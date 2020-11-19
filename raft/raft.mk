
raft: ${BINDIR}/raft ${LIBDIR}/libraft.a 

${LIBDIR}/libraft.a: raft/raft.c include/fju/raft.h 
	${CC} -c raft/raft.c ${CFLAGS} 
	${AR} rcs $@ raft.o 

${BINDIR}/raft: raft/raft-main.c ${LIBFJU}
	${CC} -o $@ raft/raft-main.c ${CFLAGS} ${LFLAGS}

PROGRAMS+=raft 
LIBRARIES+=raft
