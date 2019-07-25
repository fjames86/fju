
.PHONY: liblog 

log: liblog ${BINDIR}/fjlog 

liblog: ${BINDIR}/liblog.a

${BINDIR}/liblog.a: log/log.c 
	${CC} -c $> ${CFLAGS} 
	${AR} rcs $@ log.o


${BINDIR}/fjlog: log/fjlog.c libmmf liblog 
	${CC} -o $@ log/fjlog.c ${CFLAGS} ${LFLAGS} -llog -lmmf
