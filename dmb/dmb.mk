
dmb: ${LIBDIR}/libdmb.a 

${LIBDIR}/libdmb.a: dmb/dmb.c include/fju/dmb.h
	${CC} -c dmb/dmb.c ${CFLAGS} 
	${AR} rcs $@ dmb.o 

LIBRARIES+=dmb


