

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
CFLAGS += -Iinclude
LFLAGS += -L${BINDIR}


.PHONY: all clean ${PROJECTS}

all: ${PROJECTS}
	rm -f *.o

clean:
	rm -f ${BINDIR}/* *.o 

.for proj in ${PROJECTS}
.include "${proj}/${proj}.mk"
.endfor

