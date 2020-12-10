

fvmprogs += ${BINDIR}/cht.fvm
fvmprogs += fvm/test/test1.fvm

fvm: ${BINDIR}/fvmc ${LIBDIR}/libfvm.a ${BINDIR}/fvm fvm/test/test1.fvm ${fvmprogs}

${LIBDIR}/libfvm.a: fvm/fvm.c
	${CC} -c fvm/fvm.c ${CFLAGS} 
	${AR} rcs $@ fvm.o

${BINDIR}/fvmc: fvm/fvmc.c fvm/fvmc.h
	${CC} -o $@ fvm/fvmc.c ${CFLAGS} ${LFLAGS} 

${BINDIR}/fvm: fvm/fvm-main.c 
	${CC} -o $@ fvm/fvm-main.c ${CFLAGS} ${LFLAGS} 

fvm/test/test1.fvm: fvm/test/test1.pas ${BINDIR}/fvmc
	${BINDIR}/fvmc -o $@ -I fvm/stdlib fvm/test/test1.pas

${BINDIR}/cht.fvm: fvm/stdlib/cht.pas ${BINDIR}/fvmc
	${BINDIR}/fvmc -o $@ -I fvm/stdlib fvm/stdlib/cht.pas

LIBRARIES+=fvm
PROGRAMS+=fvm
PROGRAMS+=fvmc
