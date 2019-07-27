

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
CFLAGS += -Iinclude -g
LFLAGS += -L${LIBDIR}


.PHONY: all clean tar ${PROJECTS}

all: ${PROJECTS}
	rm -f *.o

clean:
	rm -f ${BINDIR}/* ${LIBDIR}/* *.o 

tar:
	tar -czvf fju.tar.gz ${BINDIR}/*

.for proj in ${PROJECTS}
.include "${proj}/${proj}.mk"
.endfor

