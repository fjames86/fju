
# fvm modules to build 
fvmprogs += fvm/modules/cht.fvm
fvmprogs += fvm/modules/log.fvm
fvmprogs += fvm/modules/dmb.fvm
fvmprogs += fvm/test/test1.fvm
fvmprogs += fvm/test/test.fvm
fvmprogs += fvm/test/test-dmb.fvm
fvmprogs += fvm/test/hello-world.fvm
fvmprogs += fvm/test/test-record.fvm
fvmprogs += fvm/test/test-sec.fvm
fvmprogs += fvm/test/test-file.fvm
fvmprogs += fvm/test/test-snapshot.fvm
fvmprogs += fvm/test/test-raft.fvm

fvm: ${LIBDIR}/libfvm.a ${fvmprogs}

${LIBDIR}/libfvm.a: fvm/fvm.c fvm/fvm-private.h fvm/fvm-syscall.c 
	${CC} -c fvm/fvm.c fvm/fvm-syscall.c ${CFLAGS} 
	${AR} rcs $@ fvm.o fvm-syscall.o 

# suffix rule to generate module and place it in bin/
.SUFFIXES: .pas .fvm
.pas.fvm:
	${BINDIR}/fju fvmc -o $@ -I fvm/stdlib $<
	mv $@ ${BINDIR}/

${fvmprogs}: fvm/stdlib/*.pas

LIBRARIES+=fvm

