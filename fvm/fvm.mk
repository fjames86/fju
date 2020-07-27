

fvm: ${BINDIR}/fvmc ${LIBDIR}/libfvm.a ${BINDIR}/fvm fvm/programs/test-rpc.fvm fvm/test/test1.fvm fvm/test/test2.fvm fvm/test/test3.fvm fvm/test/test6.fvm

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

fvm/programs/test-rpc.fvm: ${BINDIR}/fvmc fvm/programs/test-rpc.pas fvm/programs/test-service.pas
	${BINDIR}/fvmc -o fvm/programs/test-rpc.fvm fvm/stdlib/native.pas fvm/programs/test-rpc.pas
	${BINDIR}/fvmc -o fvm/programs/test-service.fvm fvm/stdlib/native.pas fvm/programs/test-service.pas

fvm/stdlib/native.asm: fvm/stdlib/native.pas ${BINDIR}/fvmc
	${BINDIR}/fvmc fvm/stdlib/native.pas

fvm/test/test1.fvm: ${BINDIR}/fvmc ${BINDIR}/fvm fvm/test/test1.asm fvm/stdlib/native.asm
	${BINDIR}/fvmc -o fvm/test/test1.fvm -I fvm/stdlib/ fvm/test/test1.asm
fvm/test/test2.fvm: ${BINDIR}/fvmc ${BINDIR}/fvm fvm/test/test2.asm fvm/stdlib/native.asm
	${BINDIR}/fvmc -o fvm/test/test2.fvm -I fvm/stdlib/ fvm/test/test2.asm
fvm/test/test3.fvm: ${BINDIR}/fvmc ${BINDIR}/fvm fvm/test/test3.asm fvm/stdlib/native.asm
	${BINDIR}/fvmc -o fvm/test/test3.fvm -I fvm/stdlib/ fvm/test/test3.asm	

fvm/test/test6.fvm: ${BINDIR}/fvmc fvm/test/test6.pas
	${BINDIR}/fvmc -o fvm/test/test6.fvm fvm/stdlib/native.pas fvm/test/test6.pas

PROGRAMS+=fvmc
LIBRARIES+=fvm
PROGRAMS+=fvm

