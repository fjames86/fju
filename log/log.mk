
log: ${BINDIR}/liblog.a ${BINDIR}/fjlog 

${BINDIR}/liblog.a: log/log.c include/log.h 
	${CC} -c log/log.c ${CFLAGS} 
	${AR} rcs $@ log.o


${BINDIR}/fjlog: log/fjlog.c ${BINDIR}/libmmf.a ${BINDIR}/liblog.a 
	${CC} -o $@ log/fjlog.c ${CFLAGS} ${LFLAGS} -llog -lmmf
