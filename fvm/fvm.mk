

fvm: ${BINDIR}/fvm ${LIBDIR}/libfvm.a 

${LIBDIR}/libfvm.a: fvm/fvm.c include/fju/fvm.h 
	${CC} -c -g fvm/fvm.c ${CFLAGS} 
	${AR} rcs $@ fvm.o


fvm_deps+=${LIBDIR}/libfvm.a 
${BINDIR}/fvm: fvm/fvm-main.c ${LIBFJU}
	${CC} -o $@ -g fvm/fvm-main.c ${CFLAGS} ${LFLAGS} 

PROGRAMS+=fvm 
LIBRARIES+=fvm
