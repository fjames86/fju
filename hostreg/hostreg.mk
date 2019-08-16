
hostreg: ${LIBDIR}/libhostreg.a ${LIBDIR}/libhrauth.a ${BINDIR}/hostreg

${LIBDIR}/libhostreg.a: hostreg/hostreg.c include/fju/hostreg.h 
	${CC} -c hostreg/hostreg.c ${CFLAGS} 
	${AR} rcs $@ hostreg.o

${LIBDIR}/libhrauth.a: hostreg/hrauth.c include/fju/hrauth.h 
	${CC} -c hostreg/hrauth.c ${CFLAGS} 
	${AR} rcs $@ hrauth.o

hostreg_deps += ${LIBDIR}/libmmf.a
hostreg_deps += ${LIBDIR}/librpc.a
hostreg_deps += ${LIBDIR}/libsec.a
hostreg_deps += ${LIBDIR}/libhostreg.a

${BINDIR}/hostreg: hostreg/hostreg-main.c ${LIBFJU}
	${CC} -o $@ hostreg/hostreg-main.c ${CFLAGS} ${LFLAGS} 


LIBRARIES+=hostreg
LIBRARIES+=hrauth
