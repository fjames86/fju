
freg: ${LIBDIR}/libfreg.a ${BINDIR}/clreg.fvm


${LIBDIR}/libfreg.a: freg/freg.c include/fju/freg.h freg/freg-rpc.c 
	${CC} -c ${CFLAGS} freg/freg.c freg/freg-rpc.c 
	${AR} rcs $@ freg.o freg-rpc.o 

${BINDIR}/clreg.fvm: freg/clreg.pas ${BINDIR}/fju
	${BINDIR}/fju fvmc -o ${BINDIR}/clreg.fvm -I fvm/stdlib freg/clreg.pas

LIBRARIES+=freg
