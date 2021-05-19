

dlm: ${LIBDIR}/libdlm.a 

${LIBDIR}/libdlm.a: dlm/dlm.c
	${CC} -c dlm/dlm.c ${CFLAGS} 
	${AR} rcs $@ dlm.o

LIBRARIES+=dlm


