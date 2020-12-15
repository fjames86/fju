

fvmprogs += ${BINDIR}/cht.fvm
fvmprogs += ${BINDIR}/log.fvm
fvmprogs += fvm/test/test1.fvm
fvmprogs += fvm/test/test.fvm

fvm: ${BINDIR}/fvmc ${LIBDIR}/libfvm.a ${BINDIR}/fvm fvm/test/test1.fvm ${fvmprogs}

${LIBDIR}/libfvm.a: fvm/fvm.c fvm/fvm-private.h fvm/fvm-syscall.c 
	${CC} -c fvm/fvm.c fvm/fvm-syscall.c ${CFLAGS} 
	${AR} rcs $@ fvm.o fvm-syscall.o

${BINDIR}/fvmc: fvm/fvmc.c fvm/fvm-private.h
	${CC} -o $@ fvm/fvmc.c ${CFLAGS} ${LFLAGS} 

${BINDIR}/fvm: fvm/fvm-main.c fvm/fvm-private.h
	${CC} -o $@ fvm/fvm-main.c ${CFLAGS} ${LFLAGS} 

fvm/test/test1.fvm: fvm/test/test1.pas ${BINDIR}/fvmc
	${BINDIR}/fvmc -o $@ -I fvm/stdlib fvm/test/test1.pas

fvm/test/test.fvm: fvm/test/test.pas ${BINDIR}/fvmc fvm/stdlib/string.pas fvm/stdlib/syscall.pas
	${BINDIR}/fvmc -o $@ -I fvm/stdlib fvm/test/test.pas

${BINDIR}/cht.fvm: fvm/modules/cht.pas ${BINDIR}/fvmc fvm/stdlib/syscall.pas
	${BINDIR}/fvmc -o $@ -I fvm/stdlib fvm/modules/cht.pas
${BINDIR}/log.fvm: fvm/modules/log.pas ${BINDIR}/fvmc fvm/stdlib/syscall.pas
	${BINDIR}/fvmc -o $@ -I fvm/stdlib fvm/modules/log.pas

LIBRARIES+=fvm
PROGRAMS+=fvm
PROGRAMS+=fvmc
