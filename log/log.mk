
log: ${LIBDIR}/liblog.a 

${LIBDIR}/liblog.a: log/log.c include/fju/log.h 
	${CC} -c log/log.c ${CFLAGS} 
	${AR} rcs $@ log.o

LIBRARIES+=log
