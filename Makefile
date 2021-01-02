
PROJECTS += mmf
PROJECTS += log
PROJECTS += sec
PROJECTS += hostreg
PROJECTS += hlc
PROJECTS += nls
PROJECTS += raft
PROJECTS += rpc
PROJECTS += dh
PROJECTS += lht 
PROJECTS += ef
PROJECTS += ftab
PROJECTS += freg
PROJECTS += fvm
PROJECTS += cht 
PROGRAMS=
LIBRARIES=

BINDIR=bin
LIBDIR=lib
CFLAGS=-Iinclude -Wall -fPIC -g 
LFLAGS += -L${LIBDIR} -lfju -lcrypto 


LIBFJU=${LIBDIR}/libfju.so

.PHONY: all strip clean tar install uninstall strip ${PROJECTS}

all: ${PROJECTS} ${LIBFJU} ${BINDIR}/fju
	rm -f *.o

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
${BINDIR}/fju: ${LIBFJU} ${fju_files}
	${CC} -o $@ ${CFLAGS} ${LFLAGS} fju.c ${fju_files}

clean:
	rm -f ${BINDIR}/* ${LIBDIR}/* *.o 

tar:
	tar -czvf fju.tar.gz scripts/* ${BINDIR}/* ${LIBFJU}

strip:
	strip -s ${LIBFJU} ${BINDIR}/*

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

