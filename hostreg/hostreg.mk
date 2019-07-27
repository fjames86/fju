
hostreg: ${LIBDIR}/libhostreg.a ${LIBDIR}/libhrauth.a ${BINDIR}/hostreg

${LIBDIR}/libhostreg.a: hostreg/hostreg.c include/hostreg.h 
	${CC} -c hostreg/hostreg.c ${CFLAGS} 
	${AR} rcs $@ hostreg.o

${LIBDIR}/libhrauth.a: hostreg/hrauth.c include/hrauth.h 
	${CC} -c hostreg/hrauth.c ${CFLAGS} 
	${AR} rcs $@ hrauth.o

hostreg_deps += ${LIBDIR}/libmmf.a
hostreg_deps += ${LIBDIR}/librpc.a
hostreg_deps += ${LIBDIR}/libsec.a
hostreg_deps += ${LIBDIR}/libhostreg.a

${BINDIR}/hostreg: hostreg/hostreg-main.c ${hostreg_deps}
	${CC} -o $@ hostreg/hostreg-main.c ${CFLAGS} ${LFLAGS} -lmmf -lrpc -lcrypto -lhostreg -lsec 

