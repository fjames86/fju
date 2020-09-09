
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

all: ${PROJECTS} ${LIBFJU} 
	rm -f *.o

clean:
	rm -f ${BINDIR}/* ${LIBDIR}/* *.o fvm/programs/*.fvm fvm/test/*.fvm 

tar:
	tar -czvf fju.tar.gz scripts/* ${BINDIR}/* ${LIBFJU}

strip:
	strip -s ${LIBFJU} ${BINDIR}/*

.for proj in ${PROJECTS}
.include "${proj}/${proj}.mk"
.endfor

install: all #strip
	mkdir -p /opt/fju
	cd bin && cp ${PROGRAMS} /usr/local/bin
	cp ${LIBFJU} /usr/local/lib
	mkdir -p /opt/fju/fvm
	cp fvm/programs/test-rpc.fvm /opt/fju/fvm
	sh scripts/regfvm.sh /opt/fju/fvm/test-rpc.fvm 

uninstall:
	cd /usr/local/bin && rm ${PROGRAMS}
	cd /usr/local/lib && rm libfju.so

FJU_DEPS=
FJU_LIBS=
.for lib in ${LIBRARIES}
FJU_DEPS+=${LIBDIR}/lib${lib}.a
FJU_LIBS+=-l${lib}
.endfor
${LIBFJU}: ${FJU_DEPS}
	cc -shared -o $@ -L${LIBDIR} -Wl,--whole-archive ${FJU_LIBS} -Wl,--no-whole-archive 

