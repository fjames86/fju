
fvm_flags+=fvm/stdlib/native.pas 

fvm_programs+=fvm/programs/test-rpc.fvm
fvm_programs+=fvm/programs/test-service.fvm
fvm_programs+=fvm/programs/watchdog.fvm
fvm_programs+=fvm/programs/eventlog.fvm
fvm_programs+=fvm/programs/msgq.fvm
fvm_programs+=${BINDIR}/fju.fvm

fvm_stdlib+=fvm/stdlib/native.asm
fvm_stdlib+=fvm/stdlib/xdr.asm

fvm_test+=fvm/test/test1.fvm
fvm_test+=fvm/test/test2.fvm
fvm_test+=fvm/test/test3.fvm
fvm_test+=fvm/test/test6.fvm

fvm: ${BINDIR}/fvmc ${LIBDIR}/libfvm.a ${BINDIR}/fvm ${fvm_programs} ${fvm_stdlib} ${fvm_test} ${BINDIR}/fvmc2 

libfvm_source+=fvm/fvm-module.c
libfvm_source+=fvm/fvm-state.c
libfvm_source+=fvm/fvm-opcodes.c
libfvm_source+=fvm/fvm-native.c
libfvm_source+=fvm/fvm-rpc.c
libfvm_source+=fvm/fvm-cluster.c

${LIBDIR}/libfvm.a: ${libfvm_source} include/fju/fvm.h fvm/fvm-private.h 
	${CC} -c ${libfvm_source} ${CFLAGS} 
	${AR} rcs $@ fvm-module.o fvm-state.o fvm-opcodes.o fvm-native.o fvm-rpc.o fvm-cluster.o

${BINDIR}/fvmc: fvm/fvmc.c include/fju/sec.h fvm/fvmc-pascal.c 
	${CC} -o $@ fvm/fvmc.c fvm/fvmc-pascal.c ${CFLAGS} ${LFLAGS} 

${BINDIR}/fvmc2: fvm/fvmc2.c 
	${CC} -o $@ fvm/fvmc2.c ${CFLAGS} ${LFLAGS} 

fvm_deps+=${LIBDIR}/libfvm.a
${BINDIR}/fvm: fvm/fvm-main.c ${LIBDIR}/libfvm.a 
	${CC} -o $@ fvm/fvm-main.c ${CFLAGS} ${LFLAGS} 

fvm/stdlib/native.asm: ${BINDIR}/fvmc fvm/stdlib/native.pas
	${BINDIR}/fvmc fvm/stdlib/native.pas
fvm/stdlib/xdr.asm: ${BINDIR}/fvmc fvm/stdlib/xdr.pas
	${BINDIR}/fvmc fvm/stdlib/xdr.pas

fvm/programs/test-rpc.fvm: ${BINDIR}/fvmc fvm/programs/test-rpc.pas 
	${BINDIR}/fvmc -o $@ ${fvm_flags} fvm/programs/test-rpc.pas
	rm fvm/programs/test-rpc.asm
fvm/programs/test-service.fvm: ${BINDIR}/fvmc fvm/programs/test-service.pas
	${BINDIR}/fvmc -o $@ ${fvm_flags} fvm/programs/test-service.pas
	rm fvm/programs/test-service.asm
fvm/programs/watchdog.fvm: ${BINDIR}/fvmc fvm/programs/watchdog.pas
	${BINDIR}/fvmc -o $@ ${fvm_flags} fvm/stdlib/constants.pas fvm/programs/watchdog.pas
	rm fvm/programs/watchdog.asm

fvm/programs/eventlog.fvm: ${BINDIR}/fvmc fvm/programs/eventlog.pas
	${BINDIR}/fvmc -o $@ ${fvm_flags} fvm/programs/eventlog.pas
	rm fvm/programs/eventlog.asm

fvm/programs/msgq.fvm: ${BINDIR}/fvmc fvm/programs/msgq.pas
	${BINDIR}/fvmc -o $@ ${fvm_flags} fvm/programs/msgq.pas
	rm fvm/programs/msgq.asm

${BINDIR}/fju.fvm: ${BINDIR}/fvmc fvm/stdlib/fju.pas 
	${BINDIR}/fvmc -o $@ ${fvm_flags} fvm/stdlib/constants.pas fvm/stdlib/native.pas fvm/stdlib/fju.pas
	rm fvm/stdlib/fju.asm

fvm/test/test1.fvm: ${BINDIR}/fvmc ${BINDIR}/fvm fvm/test/test1.asm fvm/stdlib/native.asm
	${BINDIR}/fvmc -o $@ -I fvm/stdlib/ fvm/test/test1.asm
fvm/test/test2.fvm: ${BINDIR}/fvmc ${BINDIR}/fvm fvm/test/test2.asm fvm/stdlib/native.asm
	${BINDIR}/fvmc -o $@ -I fvm/stdlib/ fvm/test/test2.asm
fvm/test/test3.fvm: ${BINDIR}/fvmc ${BINDIR}/fvm fvm/test/test3.asm fvm/stdlib/native.asm
	${BINDIR}/fvmc -o $@ -I fvm/stdlib/ fvm/test/test3.asm
fvm/test/test6.fvm: ${BINDIR}/fvmc fvm/test/test6.pas
	${BINDIR}/fvmc -o $@ ${fvm_flags} fvm/test/test6.pas
	rm fvm/test/test6.asm

LIBRARIES+=fvm
PROGRAMS+=fvm
PROGRAMS+=fvmc
