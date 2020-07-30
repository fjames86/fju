

fvm_programs+=fvm/programs/test-rpc.fvm
fvm_programs+=fvm/programs/test-service.fvm

fvm_stdlib+=fvm/stdlib/native.asm
fvm_stdlib+=fvm/stdlib/xdr.asm
fvm_stdlib+=fvm/stdlib/clusterinvoke.fvm

fvm_test+=fvm/test/test1.fvm
fvm_test+=fvm/test/test2.fvm
fvm_test+=fvm/test/test3.fvm
fvm_test+=fvm/test/test6.fvm

fvm: ${BINDIR}/fvmc ${LIBDIR}/libfvm.a ${BINDIR}/fvm ${fvm_programs} ${fvm_stdlib} ${fvm_test} 

libfvm_source+=fvm/fvm-module.c
libfvm_source+=fvm/fvm-state.c
libfvm_source+=fvm/fvm-opcodes.c
libfvm_source+=fvm/fvm-native.c
libfvm_source+=fvm/fvm-rpc.c
libfvm_source+=fvm/fvm-audit.c

${LIBDIR}/libfvm.a: ${libfvm_source} include/fju/fvm.h fvm/fvm-private.h 
	${CC} -c ${libfvm_source} ${CFLAGS} 
	${AR} rcs $@ fvm-module.o fvm-state.o fvm-opcodes.o fvm-native.o fvm-rpc.o fvm-audit.o

${BINDIR}/fvmc: fvm/fvmc.c include/fju/sec.h fvm/fvmc-pascal.c 
	${CC} -o $@ fvm/fvmc.c fvm/fvmc-pascal.c ${CFLAGS} ${LFLAGS} 

fvm_deps+=${LIBDIR}/libfvm.a
${BINDIR}/fvm: fvm/fvm-main.c ${LIBDIR}/libfvm.a 
	${CC} -o $@ fvm/fvm-main.c ${CFLAGS} ${LFLAGS} 

fvm/stdlib/native.asm: ${BINDIR}/fvmc fvm/stdlib/native.pas
	${BINDIR}/fvmc fvm/stdlib/native.pas
fvm/stdlib/xdr.asm: ${BINDIR}/fvmc fvm/stdlib/xdr.pas
	${BINDIR}/fvmc fvm/stdlib/xdr.pas
fvm/stdlib/clusterinvoke.fvm: ${BINDIR}/fvmc fvm/stdlib/clusterinvoke.pas 
	${BINDIR}/fvmc -o $@ fvm/stdlib/constants.pas fvm/stdlib/native.pas fvm/stdlib/clusterinvoke.pas 

fvm/programs/test-rpc.fvm: ${BINDIR}/fvmc fvm/programs/test-rpc.pas 
	${BINDIR}/fvmc -o $@ fvm/stdlib/native.pas fvm/programs/test-rpc.pas
fvm/programs/test-service.fvm: ${BINDIR}/fvmc fvm/programs/test-service.pas
	${BINDIR}/fvmc -o $@ fvm/stdlib/native.pas fvm/programs/test-service.pas

fvm/test/test1.fvm: ${BINDIR}/fvmc ${BINDIR}/fvm fvm/test/test1.asm fvm/stdlib/native.asm
	${BINDIR}/fvmc -o $@ -I fvm/stdlib/ fvm/test/test1.asm
fvm/test/test2.fvm: ${BINDIR}/fvmc ${BINDIR}/fvm fvm/test/test2.asm fvm/stdlib/native.asm
	${BINDIR}/fvmc -o $@ -I fvm/stdlib/ fvm/test/test2.asm
fvm/test/test3.fvm: ${BINDIR}/fvmc ${BINDIR}/fvm fvm/test/test3.asm fvm/stdlib/native.asm
	${BINDIR}/fvmc -o $@ -I fvm/stdlib/ fvm/test/test3.asm	
fvm/test/test6.fvm: ${BINDIR}/fvmc fvm/test/test6.pas
	${BINDIR}/fvmc -o $@ fvm/stdlib/native.pas fvm/test/test6.pas

LIBRARIES+=fvm
PROGRAMS+=fvm
PROGRAMS+=fvmc
