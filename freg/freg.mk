
freg: ${LIBDIR}/libfreg.a ${BINDIR}/freg 

${LIBDIR}/libfreg.a: freg/freg.c include/fju/freg.h freg/freg-rpc.c 
	${CC} -c ${CFLAGS} freg/freg.c freg/freg-rpc.c 
	${AR} rcs $@ freg.o freg-rpc.o 

${BINDIR}/freg: freg/freg-main.c ${LIBFJU}
	${CC} -o $@ freg/freg-main.c ${CFLAGS} ${LFLAGS}

PROGRAMS+=freg 
LIBRARIES+=freg
