
.PHONY: libhostreg libhrauth 

hostreg: libhostreg libhrauth libhostreg

libhostreg: ${BINDIR}/libhostreg.a

libhrauth: ${BINDIR}/libhrauth.a 

${BINDIR}/libhostreg.a: hostreg/hostreg.c
	${CC} -c $> ${CFLAGS} 
	${AR} rcs $@ hostreg.o

${BINDIR}/libhrauth.a: hostreg/hrauth.c
	${CC} -c $> ${CFLAGS} 
	${AR} rcs $@ hrauth.o

${BINDIR}/hostreg: hostreg/hostreg-main.c libmmf librpc libsec libhostreg 
	${CC} -o $@ hostreg/hostreg-main.c ${CFLAGS} ${LFLAGS} -lmmf -lrpc -lcrypto -lhostreg -lsec 

