
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
fvmprogs += fvm/test/native-test.fvm
fvmprogs += fvm/modules/fvm-cluster.fvm
fvmprogs += fvm/modules/stateseq.fvm
fvmprogs += fvm/test/test-xcall.fvm

fvm: ${LIBDIR}/libfvm.a ${fvmprogs} fvm/modules/fvm-cluster.c

${LIBDIR}/libfvm.a: fvm/fvm.c fvm/fvm-private.h fvm/fvm-syscall.c 
	${CC} -c fvm/fvm.c fvm/fvm-syscall.c ${CFLAGS} 
	${AR} rcs $@ fvm.o fvm-syscall.o 

fvm/modules/fvm-cluster.c: fvm/modules/fvm-cluster.fvm ${BINDIR}/fju 
	${BINDIR}/fju fvmc -C -o $@ fvm/modules/fvm-cluster.fvm
FVMMODULES += fvm/modules/fvm-cluster.c

fvm/stdlib/dmb.pas: include/fju/dmb-category.h
	echo "" > fvm/stdlib/dmb.pas
	echo "{ -*- mode: fvm -*- }" >> fvm/stdlib/dmb.pas
	echo "" >> fvm/stdlib/dmb.pas
	echo "{ Generated from include/fju/dmb-category.h - DO NOT EDIT }" >> fvm/stdlib/dmb.pas
	echo "" >> fvm/stdlib/dmb.pas
	grep "define DMB_CAT_" include/fju/dmb-category.h | tr '[:upper:]' '[:lower:]' | sed 's/#define //' | perl -pe 's/(^|_)./uc($$&)/ge;s/_//g' | awk '{print "Const " $$1 " = " $$2 ";" }' >> fvm/stdlib/dmb.pas
	echo "" >> fvm/stdlib/dmb.pas

fvm/stdlib/programs.pas: include/fju/programs.h
	echo "" > fvm/stdlib/programs.pas
	echo "{ -*- mode: fvm -*- }" >> fvm/stdlib/programs.pas
	echo "" >> fvm/stdlib/programs.pas
	echo "{ Generated from include/fju/programs.h - DO NOT EDIT }" >> fvm/stdlib/programs.pas
	echo "" >> fvm/stdlib/programs.pas
	echo "Const FjuBaseProg = 0x2FFF7770;" >> fvm/stdlib/programs.pas
	grep "_RPC_" include/fju/programs.h | tr '[:upper:]' '[:lower:]' | sed 's/#define //' | perl -pe 's/(^|_)./uc($$&)/ge;s/_//g' | awk '{print "Const " $$1 " = " $$2 " " $$3 " " $$4 ";" }' >> fvm/stdlib/programs.pas
	echo "" >> fvm/stdlib/programs.pas


# suffix rule to generate module and place it in bin/
.SUFFIXES: .pas .fvm
.pas.fvm:
	${BINDIR}/fju fvmc -o $@ -I fvm/stdlib $<
	@cp $@ ${BINDIR}/

${fvmprogs}: ${BINDIR}/fju fvm/stdlib/*.pas fvm/stdlib/dmb.pas fvm/stdlib/programs.pas

LIBRARIES+=fvm

