

rpc: ${LIBDIR}/librpc.a 

${LIBDIR}/librpc.a: rpc/rpc.c rpc/rpcd.c rpc/shauth.c include/fju/rpc.h include/fju/rpcd.h include/fju/shauth.h rpc/rpc-private.h 
	${CC} -c rpc/rpc.c rpc/rpcd.c rpc/shauth.c ${CFLAGS} 
	${AR} rcs $@ rpc.o rpcd.o shauth.o 

LIBRARIES+=rpc


