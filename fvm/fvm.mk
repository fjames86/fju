

fvm: ${BINDIR}/fvmc ${LIBDIR}/libfvm.a ${BINDIR}/fvm 

${LIBDIR}/libfvm.a: fvm/fvm-module.c fvm/fvm-state.c fvm/fvm-opcodes.c include/fju/fvm.h fvm/fvm-native.c fvm/fvm-private.h fvm/fvm-rpc.c 
	${CC} -c -g fvm/fvm-module.c fvm/fvm-state.c fvm/fvm-opcodes.c fvm/fvm-native.c fvm/fvm-rpc.c ${CFLAGS} 
	${AR} rcs $@ fvm-module.o fvm-state.o fvm-opcodes.o fvm-native.o fvm-rpc.o 

fvmc_deps+=
${BINDIR}/fvmc: fvm/fvmc.c
	${CC} -o $@ -g fvm/fvmc.c ${CFLAGS} ${LFLAGS} 

fvm_deps+=${LIBDIR}/libfvm.a
${BINDIR}/fvm: fvm/fvm-main.c ${LIBDIR}/libfvm.a 
	${CC} -o $@ -g fvm/fvm-main.c ${CFLAGS} ${LFLAGS} 

PROGRAMS+=fvmc
LIBRARIES+=fvm
PROGRAMS+=fvm
