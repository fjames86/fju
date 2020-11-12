
raft: ${BINDIR}/raft ${LIBDIR}/libraft.a ${LIBDIR}/librex.so ${BINDIR}/raft2

${LIBDIR}/libraft.a: raft/raft.c raft/raft-rpc.c include/fju/raft.h raft/raft2.c 
	${CC} -c raft/raft.c raft/raft-rpc.c raft/raft2.c ${CFLAGS} 
	${AR} rcs $@ raft.o raft-rpc.o raft2.o

${LIBDIR}/librex.so: raft/rex.c include/fju/rex.h 
	${CC} -shared -o $@ raft/rex.c ${CFLAGS} -L${LIBDIR} -lfju 

${BINDIR}/raft: raft/raft-main.c ${LIBFJU}
	${CC} -o $@ raft/raft-main.c ${CFLAGS} ${LFLAGS}

${BINDIR}/raft2: raft/raft2-main.c ${LIBFJU}
	${CC} -o $@ raft/raft2-main.c ${CFLAGS} ${LFLAGS}

PROGRAMS+=raft 
LIBRARIES+=raft
PROGRAMS+=raft2 
