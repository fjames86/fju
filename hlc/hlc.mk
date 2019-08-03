
hlc: ${BINDIR}/hlc ${LIBDIR}/libhlc.a 

${LIBDIR}/libhlc.a: hlc/hlc.c include/hlc.h 
	${CC} -c hlc/hlc.c ${CFLAGS} 
	${AR} rcs $@ hlc.o 

hlc_deps += ${LIBDIR}/libmmf.a
hlc_deps += ${LIBDIR}/libsec.a
hlc_deps += ${LIBDIR}/liblog.a
hlc_deps += ${LIBDIR}/libnls.a
hlc_deps += ${LIBDIR}/libhlc.a

${BINDIR}/hlc: hlc/hlc-main.c ${hlc_deps}
	${CC} -o $@ hlc/hlc-main.c ${CFLAGS} ${LFLAGS} -lmmf -lcrypto -llog -lsec -lnls -lhlc 
