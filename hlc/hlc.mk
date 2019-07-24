
hlc: ${BINDIR}/libhlc.a ${BINDIR}/hlc  

${BINDIR}/libhlc.a: hlc/hlc.c
	${CC} -c $> ${CFLAGS} 
	${AR} rcs $@ hlc.o 

${BINDIR}/hlc: hlc/hlc-main.c mmf sec log nls ${BINDIR}/libhlc.a 
	${CC} -o $@ hlc/hlc-main.c ${CFLAGS} ${LFLAGS} -lmmf -lcrypto -llog -lsec -lnls -lhlc 
