

fvm: ${BINDIR}/fvmc ${LIBDIR}/libfvm.a ${BINDIR}/fvm 

${LIBDIR}/libfvm.a: fvm/fvm.c
	${CC} -c fvm/fvm.c ${CFLAGS} 
	${AR} rcs $@ fvm.o

${BINDIR}/fvmc: fvm/fvmc.c fvm/fvmc.h
	${CC} -o $@ fvm/fvmc.c ${CFLAGS} ${LFLAGS} 

${BINDIR}/fvm: fvm/fvm-main.c 
	${CC} -o $@ fvm/fvm-main.c ${CFLAGS} ${LFLAGS} 


LIBRARIES+=fvm
PROGRAMS+=fvm
PROGRAMS+=fvmc
