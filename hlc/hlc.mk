
hlc: ${BINDIR}/hlc ${BINDIR}/libhlc.a 

${BINDIR}/libhlc.a: hlc/hlc.c include/hlc.h 
	${CC} -c hlc/hlc.c ${CFLAGS} 
	${AR} rcs $@ hlc.o 

hlc_deps += ${BINDIR}/libmmf.a
hlc_deps += ${BINDIR}/libsec.a
hlc_deps += ${BINDIR}/liblog.a
hlc_deps += ${BINDIR}/libnls.a
hlc_deps += ${BINDIR}/libhlc.a

${BINDIR}/hlc: hlc/hlc-main.c ${hlc_deps}
	${CC} -o $@ hlc/hlc-main.c ${CFLAGS} ${LFLAGS} -lmmf -lcrypto -llog -lsec -lnls -lhlc 
