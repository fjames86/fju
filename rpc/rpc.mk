

rpc: ${BINDIR}/fjud ${LIBDIR}/librpc.a 

${LIBDIR}/librpc.a: rpc/rpc.c rpc/rpcd.c rpc/shauth.c include/fju/rpc.h include/fju/rpcd.h include/fju/shauth.h rpc/rpc-private.h 
	${CC} -c rpc/rpc.c rpc/rpcd.c rpc/shauth.c ${CFLAGS} 
	${AR} rcs $@ rpc.o rpcd.o shauth.o 

${BINDIR}/fjud: rpc/fjud-main.c rpc/cmdprog.c rpc/rpc-private.h ${LIBFJU} ${FVMMODULES}
	${CC} -o $@ rpc/fjud-main.c rpc/cmdprog.c ${FVMMODULES} ${CFLAGS} ${LFLAGS} 

PROGRAMS+=fjud
LIBRARIES+=rpc


