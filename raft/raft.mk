
raft: ${LIBDIR}/libraft.a ${BINDIR}/raft-mgt.fvm


${LIBDIR}/libraft.a: raft/raft.c include/fju/raft.h raft/raft-example.c 
	${CC} -c raft/raft.c raft/raft-example.c ${CFLAGS} 
	${AR} rcs $@ raft.o raft-example.o 

${BINDIR}/raft-mgt.fvm: ${BINDIR}/fju raft/raft-mgt.pas
	${BINDIR}/fju fvmc -o $@ -I fvm/stdlib raft/raft-mgt.pas

raft/raft-mgt.c: ${BINDIR}/raft-mgt.fvm ${BINDIR}/fju
	${BINDIR}/fju fvmc -o raft/raft-mgt.c -C ${BINDIR}/raft-mgt.fvm

LIBRARIES+=raft
FVMMODULES+=raft/raft-mgt.c
