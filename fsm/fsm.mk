
fsm: ${LIBDIR}/libfsm.a 

${LIBDIR}/libfsm.a: fsm/fsm.c include/fju/fsm.h
	${CC} -c fsm/fsm.c ${CFLAGS} 
	${AR} rcs $@ fsm.o 

LIBRARIES+=fsm

