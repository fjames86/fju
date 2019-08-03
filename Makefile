

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

BINDIR=bin
LIBDIR=lib
CFLAGS += -Iinclude -g -Wall
LFLAGS += -L${LIBDIR}


.PHONY: all clean tar install ${PROJECTS}

all: ${PROJECTS}
	rm -f *.o

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



