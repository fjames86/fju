
log: ${LIBDIR}/liblog.a ${BINDIR}/fjlog 

${LIBDIR}/liblog.a: log/log.c include/fju/log.h 
	${CC} -c log/log.c ${CFLAGS} 
	${AR} rcs $@ log.o


${BINDIR}/fjlog: log/fjlog.c ${LIBFJU}
	${CC} -o $@ log/fjlog.c ${CFLAGS} ${LFLAGS} 

LIBRARIES+=log
