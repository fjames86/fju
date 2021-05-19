
freg: ${LIBDIR}/libfreg.a

${LIBDIR}/libfreg.a: freg/freg.c include/fju/freg.h freg/freg-rpc.c 
	${CC} -c ${CFLAGS} freg/freg.c freg/freg-rpc.c 
	${AR} rcs $@ freg.o freg-rpc.o 

LIBRARIES+=freg
