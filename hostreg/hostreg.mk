
hostreg: ${BINDIR}/libhostreg.a ${BINDIR}/libhrauth.a ${BINDIR}/hostreg

${BINDIR}/libhostreg.a: hostreg/hostreg.c include/hostreg.h 
	${CC} -c hostreg/hostreg.c ${CFLAGS} 
	${AR} rcs $@ hostreg.o

${BINDIR}/libhrauth.a: hostreg/hrauth.c include/hrauth.h 
	${CC} -c hostreg/hrauth.c ${CFLAGS} 
	${AR} rcs $@ hrauth.o

hostreg_deps += ${BINDIR}/libmmf.a
hostreg_deps += ${BINDIR}/librpc.a
hostreg_deps += ${BINDIR}/libsec.a
hostreg_deps += ${BINDIR}/libhostreg.a

${BINDIR}/hostreg: hostreg/hostreg-main.c ${hostreg_deps}
	${CC} -o $@ hostreg/hostreg-main.c ${CFLAGS} ${LFLAGS} -lmmf -lrpc -lcrypto -lhostreg -lsec 

