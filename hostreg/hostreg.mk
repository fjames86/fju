
hostreg: ${BINDIR}/libhostreg.a ${BINDIR}/hostreg ${BINDIR}/libhrauth.a 

${BINDIR}/libhostreg.a: hostreg/hostreg.c
	${CC} -c $> ${CFLAGS} 
	${AR} rcs $@ hostreg.o

${BINDIR}/libhrauth.a: hostreg/hrauth.c
	${CC} -c $> ${CFLAGS} 
	${AR} rcs $@ hrauth.o

${BINDIR}/hostreg: hostreg/hostreg.c mmf rpc sec ${BINDIR}/libhostreg.a 
	${CC} -o $@ hostreg/hostreg-main.c ${CFLAGS} ${LFLAGS} -lmmf -lrpc -lcrypto -lhostreg -lsec 

