
PROJECTS += mmf
PROJECTS += log
PROJECTS += sec
PROJECTS += hostreg
PROJECTS += hlc
PROJECTS += nls
PROJECTS += lht 
PROJECTS += raft
PROJECTS += rpc
PROJECTS += dh
PROJECTS += ef
PROJECTS += freg
PROJECTS += ftab
PROJECTS += fvm
PROJECTS += cht
PROJECTS += dmb
PROJECTS += dlm
PROJECTS += fsm
PROGRAMS=
LIBRARIES=

BINDIR=bin
LIBDIR=lib
CFLAGS=-Iinclude -Wall -fPIC -g 
LFLAGS += -lcrypto 
LFLAGS += -ldl -laio # Linux only 

LIBFJU=${LIBDIR}/libfju.so

.PHONY: all strip clean tar install uninstall strip ${PROJECTS} installer 

all: ${PROJECTS} ${LIBFJU} ${BINDIR}/fju ${BINDIR}/fjud 
	rm -f *.o

clean:
	rm -f ${BINDIR}/* ${LIBDIR}/* *.o fvm/test/*.fvm ${FVMMODULES}

tar:
	tar -czvf fju.tar.gz scripts/* ${BINDIR}/* ${LIBFJU}

strip:
	strip -s ${LIBFJU} ${BINDIR}/fju ${BINDIR}/fjud ${BINDIR}/fjlic 

.for proj in ${PROJECTS}
.include "${proj}/${proj}.mk"
.endfor

install: all #strip
	mkdir -p /opt/fju
	sh scripts/fjud.sh stop 
	cp bin/fju bin/fjud /usr/local/bin
	cp ${LIBFJU} /usr/local/lib
	mkdir -p /opt/fju/fvm
	cp ${BINDIR}/*.fvm /opt/fju/fvm
	for fname in $$(find /opt/fju/fvm -name \*.fvm); do sh scripts/regfvm.sh -p $$fname; done 

uninstall:
	cd /usr/local/bin && rm fju fjud 
	cd /usr/local/lib && rm libfju.so

FJU_DEPS=
FJU_LIBS=
.for lib in ${LIBRARIES}
FJU_DEPS+=${LIBDIR}/lib${lib}.a
FJU_LIBS+=-l${lib}
.endfor
${LIBFJU}: ${FJU_DEPS}
	cc -shared -o $@ -L${LIBDIR} -Wl,--whole-archive ${FJU_LIBS} -Wl,--no-whole-archive

fju_files += fju.c 
fju_files += log/fjlog.c
fju_files += rpc/rpclt.c
fju_files += rpc/xdru.c
fju_files += raft/raft-main.c
fju_files += cht/cht-main.c
fju_files += dh/ecdh.c
fju_files += sec/shamir.c
fju_files += fvm/fvm-main.c
fju_files += fvm/fvmc.c
fju_files += freg/freg-main.c
fju_files += hostreg/hostreg-main.c
fju_files += dmb/dmb-main.c
fju_files += fsm/fsm-main.c
fju_files += dlm/dlm-main.c
${BINDIR}/fju: ${FJU_DEPS} ${fju_files}
	${CC} -o $@ ${CFLAGS} ${fju_files} -L${LIBDIR} ${FJU_LIBS} ${LFLAGS}


fjud_files += fjud.c 
fjud_files += rpc/cmdprog.c
fjud_files += ${FVMMODULES}
fjud_files += fvm/test/native.c
fjud_files += fvm/test/test-aio.c 
${BINDIR}/fjud: ${FJU_DEPS} ${fjud_files}
	${CC} -o $@ ${CFLAGS} ${fjud_files} -L${LIBDIR} ${FJU_LIBS} ${LFLAGS}

installer: ${BINDIR}/fju ${BINDIR}/fjud ${LIBFJU}
	mkdir -p opt/fju/bin
	mkdir -p opt/fju/lib
	mkdir -p opt/fju/fvm 
	cp ${BINDIR}/fju ${BINDIR}/fjud opt/fju/bin
	cp ${LIBFJU} opt/fju/lib
	cp ${BINDIR}/*.fvm opt/fju/fvm
	cp scripts/freg-defaults.reg opt/fju/freg-defaults.reg 
	tar czvf fju.tar.gz opt
	rm -rf opt
	cat scripts/install.sh fju.tar.gz > bin/install.sh

