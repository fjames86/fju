

fvm2: ${BINDIR}/fvmc ${LIBDIR}/libfvm2.a ${BINDIR}/fvm2 

${LIBDIR}/libfvm2.a: fvm2/fvm2-module.c fvm2/fvm2-state.c fvm2/fvm2-opcodes.c include/fju/fvm2.h 
	${CC} -c -g fvm2/fvm2-module.c fvm2/fvm2-state.c fvm2/fvm2-opcodes.c ${CFLAGS} 
	${AR} rcs $@ fvm2-module.o fvm2-state.o fvm2-opcodes.o

fvmc_deps+=
${BINDIR}/fvmc: fvm2/fvmc.c
	${CC} -o $@ -g fvm2/fvmc.c ${CFLAGS} ${LFLAGS} 

fvm2_deps+=${LIBDIR}/libfvm2.a
${BINDIR}/fvm2: fvm2/fvm2-main.c ${LIBDIR}/libfvm2.a 
	${CC} -o $@ -g fvm2/fvm2-main.c ${CFLAGS} ${LFLAGS} 

PROGRAMS+=fvmc
LIBRARIES+=fvm2
PROGRAMS+=fvm2
