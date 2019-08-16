
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
LIBRARIES=

BINDIR=bin
LIBDIR=lib
CFLAGS=-Iinclude -Wall -fPIC
LFLAGS += -L${LIBDIR} -lfju -lcrypto 


LIBFJU=${LIBDIR}/libfju.so

.PHONY: all clean tar install ${PROJECTS}

all: ${PROJECTS} ${LIBFJU}
	rm -f *.o
	strip -s ${LIBFJU} bin/*

clean:
	rm -f ${BINDIR}/* ${LIBDIR}/* *.o 

tar:
	tar -czvf fju.tar.gz rpcd.sh ${BINDIR}/*

.for proj in ${PROJECTS}
.include "${proj}/${proj}.mk"
.endfor

install:
	mkdir -p /opt/fju/bin
	cp bin/* /opt/fju/bin
	cp rpcd.sh /opt/fju 

FJU_DEPS=
FJU_LIBS=
.for lib in ${LIBRARIES}
FJU_DEPS+=${LIBDIR}/lib${lib}.a
FJU_LIBS+=-l${lib}
.endfor
${LIBFJU}: ${FJU_DEPS}
	cc -shared -o $@ -L${LIBDIR} -Wl,--whole-archive ${FJU_LIBS} -Wl,--no-whole-archive 
