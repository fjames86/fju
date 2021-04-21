

fvmprogs += ${BINDIR}/cht.fvm
fvmprogs += ${BINDIR}/log.fvm
fvmprogs += ${BINDIR}/dmb.fvm

fvmprogs += ${BINDIR}/test1.fvm
fvmprogs += ${BINDIR}/test.fvm
fvmprogs += ${BINDIR}/test-dmb.fvm
fvmprogs += ${BINDIR}/hello-world.fvm
fvmprogs += ${BINDIR}/test-record.fvm
fvmprogs += ${BINDIR}/test-sec.fvm
fvmprogs += ${BINDIR}/test-file.fvm
fvmprogs += ${BINDIR}/test-snapshot.fvm
fvmprogs += ${BINDIR}/test-raft.fvm

fvm: ${LIBDIR}/libfvm.a ${fvmprogs}

${LIBDIR}/libfvm.a: fvm/fvm.c fvm/fvm-private.h fvm/fvm-syscall.c 
	${CC} -c fvm/fvm.c fvm/fvm-syscall.c ${CFLAGS} 
	${AR} rcs $@ fvm.o fvm-syscall.o 

${BINDIR}/cht.fvm: fvm/modules/cht.pas ${BINDIR}/fju fvm/stdlib/*.pas
	${BINDIR}/fju fvmc -o $@ -I fvm/stdlib fvm/modules/cht.pas
${BINDIR}/log.fvm: fvm/modules/log.pas ${BINDIR}/fju fvm/stdlib/*.pas
	${BINDIR}/fju fvmc -o $@ -I fvm/stdlib fvm/modules/log.pas
${BINDIR}/dmb.fvm: fvm/modules/dmb.pas ${BINDIR}/fju fvm/stdlib/*.pas
	${BINDIR}/fju fvmc -o $@ -I fvm/stdlib fvm/modules/dmb.pas

${BINDIR}/test1.fvm: fvm/test/test1.pas ${BINDIR}/fju
	${BINDIR}/fju fvmc -o $@ -I fvm/stdlib fvm/test/test1.pas
${BINDIR}/test.fvm: fvm/test/test.pas ${BINDIR}/fju fvm/stdlib/string.pas fvm/stdlib/*.pas
	${BINDIR}/fju fvmc -o $@ -I fvm/stdlib fvm/test/test.pas
${BINDIR}/test-dmb.fvm: fvm/test/test-dmb.pas fvm/stdlib/*.pas
	${BINDIR}/fju fvmc -o $@ -I fvm/stdlib fvm/test/test-dmb.pas
${BINDIR}/test-record.fvm: fvm/test/test-record.pas ${BINDIR}/fju fvm/stdlib/*.pas
	${BINDIR}/fju fvmc -o $@ -I fvm/stdlib fvm/test/test-record.pas
${BINDIR}/test-sec.fvm: fvm/test/test-sec.pas ${BINDIR}/fju fvm/stdlib/*.pas
	${BINDIR}/fju fvmc -o $@ -I fvm/stdlib fvm/test/test-sec.pas
${BINDIR}/test-file.fvm: fvm/test/test-file.pas ${BINDIR}/fju fvm/stdlib/*.pas
	${BINDIR}/fju fvmc -o $@ -I fvm/stdlib fvm/test/test-file.pas
${BINDIR}/test-snapshot.fvm: fvm/test/test-snapshot.pas ${BINDIR}/fju fvm/stdlib/*.pas
	${BINDIR}/fju fvmc -o $@ -I fvm/stdlib fvm/test/test-snapshot.pas
${BINDIR}/hello-world.fvm: fvm/test/hello-world.pas ${BINDIR}/fju fvm/stdlib/*.pas
	${BINDIR}/fju fvmc -o $@ -I fvm/stdlib fvm/test/hello-world.pas
${BINDIR}/test-raft.fvm: fvm/test/test-raft.pas ${BINDIR}/fju fvm/stdlib/*.pas
	${BINDIR}/fju fvmc -o $@ -I fvm/stdlib fvm/test/test-raft.pas

LIBRARIES+=fvm

