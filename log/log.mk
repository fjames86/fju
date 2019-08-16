
log: ${LIBDIR}/liblog.a ${BINDIR}/fjlog 

${LIBDIR}/liblog.a: log/log.c include/fju/log.h 
	${CC} -c log/log.c ${CFLAGS} 
	${AR} rcs $@ log.o


${BINDIR}/fjlog: log/fjlog.c ${LIBDIR}/libmmf.a ${LIBDIR}/liblog.a 
	${CC} -o $@ log/fjlog.c ${CFLAGS} ${LFLAGS} -llog -lmmf

LIBRARIES+=log
