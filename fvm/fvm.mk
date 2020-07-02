

fvm: ${BINDIR}/fvmc ${LIBDIR}/libfvm.a ${BINDIR}/fvm fvm/programs/test-rpc.fvm fvm/test/test1.fvm

${LIBDIR}/libfvm.a: fvm/fvm-module.c fvm/fvm-state.c fvm/fvm-opcodes.c include/fju/fvm.h fvm/fvm-native.c fvm/fvm-private.h fvm/fvm-rpc.c 
	${CC} -c fvm/fvm-module.c fvm/fvm-state.c fvm/fvm-opcodes.c fvm/fvm-native.c fvm/fvm-rpc.c ${CFLAGS} 
	${AR} rcs $@ fvm-module.o fvm-state.o fvm-opcodes.o fvm-native.o fvm-rpc.o 

fvmc_deps+=
${BINDIR}/fvmc: fvm/fvmc.c
	${CC} -o $@ fvm/fvmc.c ${CFLAGS} ${LFLAGS} 

fvm_deps+=${LIBDIR}/libfvm.a
${BINDIR}/fvm: fvm/fvm-main.c ${LIBDIR}/libfvm.a 
	${CC} -o $@ fvm/fvm-main.c ${CFLAGS} ${LFLAGS} 

fvm/programs/test-rpc.fvm: ${BINDIR}/fvmc fvm/programs/test-rpc.asm
	${BINDIR}/fvmc -o fvm/programs/test-rpc.fvm -I fvm/stdlib/ fvm/programs/test-rpc.asm 

fvm/test/test1.fvm: ${BINDIR}/fvmc fvm/test/test1.asm ${BINDIR}/fvm fvm/test/test2.asm
	${BINDIR}/fvmc -I fvm/stdlib/ -o fvm/test/test1.fvm fvm/test/test1.asm 
	${BINDIR}/fvmc -I fvm/stdlib/ -o fvm/test/test2.fvm fvm/test/test2.asm 


PROGRAMS+=fvmc
LIBRARIES+=fvm
PROGRAMS+=fvm

