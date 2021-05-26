
raft: ${LIBDIR}/libraft.a ${BINDIR}/raft-mgt.fvm


${LIBDIR}/libraft.a: raft/raft.c include/fju/raft.h raft/raft-example.c 
	${CC} -c raft/raft.c raft/raft-example.c ${CFLAGS} 
	${AR} rcs $@ raft.o raft-example.o 

${BINDIR}/raft-mgt.fvm: ${BINDIR}/fju raft/raft-mgt.pas
	${BINDIR}/fju fvmc -o $@ -I fvm/stdlib raft/raft-mgt.pas

LIBRARIES+=raft
