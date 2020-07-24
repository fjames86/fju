

fvm: ${BINDIR}/fvmc ${LIBDIR}/libfvm.a ${BINDIR}/fvm fvm/programs/test-rpc.fvm fvm/test/test1.fvm 

libfvm_source+=fvm/fvm-module.c
libfvm_source+=fvm/fvm-state.c
libfvm_source+=fvm/fvm-opcodes.c
libfvm_source+=fvm/fvm-native.c
libfvm_source+=fvm/fvm-rpc.c
libfvm_source+=fvm/fvm-audit.c

${LIBDIR}/libfvm.a: ${libfvm_source} include/fju/fvm.h fvm/fvm-private.h 
	${CC} -c ${libfvm_source} ${CFLAGS} 
	${AR} rcs $@ fvm-module.o fvm-state.o fvm-opcodes.o fvm-native.o fvm-rpc.o fvm-audit.o

fvmc_deps+=
${BINDIR}/fvmc: fvm/fvmc.c include/fju/sec.h fvm/fvmc-pascal.c 
	${CC} -o $@ fvm/fvmc.c fvm/fvmc-pascal.c ${CFLAGS} ${LFLAGS} 

fvm_deps+=${LIBDIR}/libfvm.a
${BINDIR}/fvm: fvm/fvm-main.c ${LIBDIR}/libfvm.a 
	${CC} -o $@ fvm/fvm-main.c ${CFLAGS} ${LFLAGS} 

fvm/programs/test-rpc.fvm: ${BINDIR}/fvmc fvm/programs/test-rpc.asm fvm/programs/test-service.asm
	${BINDIR}/fvmc -o fvm/programs/test-rpc.fvm -I fvm/stdlib/ fvm/programs/test-rpc.asm
	${BINDIR}/fvmc -o fvm/programs/test-service.fvm -I fvm/stdlib/ fvm/programs/test-service.asm

fvmtest_source+=fvm/test/test1.asm
fvmtest_source+=fvm/test/test2.asm
fvmtest_source+=fvm/test/test3.asm
fvmtest_source+=fvm/test/test4.asm
fvmtest_source+=fvm/test/test5.asm
fvm/test/test1.fvm: ${BINDIR}/fvmc ${BINDIR}/fvm ${fvmtest_source}
	for x in ${fvmtest_source}; do ${BINDIR}/fvmc -I fvm/stdlib/ $$x; done 


PROGRAMS+=fvmc
LIBRARIES+=fvm
PROGRAMS+=fvm

