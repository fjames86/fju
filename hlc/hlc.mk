
.PHONY: libhlc

hlc: ${BINDIR}/hlc libhlc

libhlc: ${BINDIR}/libhlc.a 

${BINDIR}/libhlc.a: hlc/hlc.c libmmf libsec liblog libnls 
	${CC} -c hlc/hlc.c ${CFLAGS} 
	${AR} rcs $@ hlc.o 

${BINDIR}/hlc: hlc/hlc-main.c libmmf libsec liblog libnls libhlc 
	${CC} -o $@ hlc/hlc-main.c ${CFLAGS} ${LFLAGS} -lmmf -lcrypto -llog -lsec -lnls -lhlc 
