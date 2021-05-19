
hostreg: ${LIBDIR}/libhostreg.a ${LIBDIR}/libhrauth.a 

${LIBDIR}/libhostreg.a: hostreg/hostreg.c include/fju/hostreg.h 
	${CC} -c hostreg/hostreg.c ${CFLAGS} 
	${AR} rcs $@ hostreg.o

${LIBDIR}/libhrauth.a: hostreg/hrauth.c include/fju/hrauth.h 
	${CC} -c hostreg/hrauth.c ${CFLAGS} 
	${AR} rcs $@ hrauth.o

LIBRARIES+=hostreg
LIBRARIES+=hrauth
