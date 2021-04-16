

fvmprogs += ${BINDIR}/cht.fvm
fvmprogs += ${BINDIR}/log.fvm
fvmprogs += ${BINDIR}/dmb.fvm

fvmprogs += fvm/test/test1.fvm
fvmprogs += fvm/test/test.fvm
fvmprogs += fvm/test/test-dmb.fvm
fvmprogs += fvm/test/hello-world.fvm
fvmprogs += fvm/test/test-record.fvm
fvmprogs += fvm/test/test-sec.fvm
fvmprogs += fvm/test/test-file.fvm
fvmprogs += fvm/test/test-snapshot.fvm

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

fvm/test/test1.fvm: fvm/test/test1.pas ${BINDIR}/fju
	${BINDIR}/fju fvmc -o $@ -I fvm/stdlib fvm/test/test1.pas
fvm/test/test.fvm: fvm/test/test.pas ${BINDIR}/fju fvm/stdlib/string.pas fvm/stdlib/*.pas
	${BINDIR}/fju fvmc -o $@ -I fvm/stdlib fvm/test/test.pas
fvm/test/test-dmb.fvm: fvm/test/test-dmb.pas fvm/stdlib/*.pas
	${BINDIR}/fju fvmc -o $@ -I fvm/stdlib fvm/test/test-dmb.pas
fvm/test/test-record.fvm: fvm/test/test-record.pas ${BINDIR}/fju fvm/stdlib/*.pas
	${BINDIR}/fju fvmc -o $@ -I fvm/stdlib fvm/test/test-record.pas
fvm/test/test-sec.fvm: fvm/test/test-sec.pas ${BINDIR}/fju fvm/stdlib/*.pas
	${BINDIR}/fju fvmc -o $@ -I fvm/stdlib fvm/test/test-sec.pas
fvm/test/test-file.fvm: fvm/test/test-file.pas ${BINDIR}/fju fvm/stdlib/*.pas
	${BINDIR}/fju fvmc -o $@ -I fvm/stdlib fvm/test/test-file.pas
fvm/test/test-snapshot.fvm: fvm/test/test-snapshot.pas ${BINDIR}/fju fvm/stdlib/*.pas
	${BINDIR}/fju fvmc -o $@ -I fvm/stdlib fvm/test/test-snapshot.pas
fvm/test/hello-world.fvm: fvm/test/hello-world.pas ${BINDIR}/fju fvm/stdlib/*.pas
	${BINDIR}/fju fvmc -o $@ -I fvm/stdlib fvm/test/hello-world.pas

LIBRARIES+=fvm

