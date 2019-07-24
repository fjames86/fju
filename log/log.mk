
log: ${BINDIR}/liblog.a ${BINDIR}/fjlog 

${BINDIR}/liblog.a: log/log.c 
	${CC} -c $> ${CFLAGS} 
	${AR} rcs $@ log.o


${BINDIR}/fjlog: log/fjlog.c mmf 
	${CC} -o $@ log/fjlog.c ${CFLAGS} ${LFLAGS} -llog -lmmf
