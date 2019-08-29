
raft: ${BINDIR}/raft ${LIBDIR}/libraft.a ${LIBDIR}/librex.so 

${LIBDIR}/libraft.a: raft/raft.c raft/raft-rpc.c include/fju/raft.h 
	${CC} -c raft/raft.c raft/raft-rpc.c ${CFLAGS} 
	${AR} rcs $@ raft.o raft-rpc.o 

${LIBDIR}/librex.so: raft/rex.c include/fju/rex.h 
	${CC} -shared -o $@ raft/rex.c ${CFLAGS} -L${LIBDIR} -lfju 

${BINDIR}/raft: raft/raft-main.c ${LIBFJU}
	${CC} -o $@ raft/raft-main.c ${CFLAGS} ${LFLAGS}

PROGRAMS+=raft 
LIBRARIES+=raft

