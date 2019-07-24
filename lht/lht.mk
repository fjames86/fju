
lht: ${BINDIR}/liblht.a ${BINDIR}/lht 

${BINDIR}/liblht.a: lht/lht.c 
	${CC} -c $> ${CFLAGS} 
	${AR} rcs $@ lht.o


${BINDIR}/lht: lht/lht-main.c mmf ${BINDIR}/liblht.a ${BINDIR}/liblog.a 
	${CC} -o $@ lht/lht-main.c ${CFLAGS} ${LFLAGS} -llog -lmmf -llht 
