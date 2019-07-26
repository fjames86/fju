
lht: ${BINDIR}/lht ${BINDIR}/liblht.a 

${BINDIR}/liblht.a: lht/lht.c 
	${CC} -c $> ${CFLAGS} 
	${AR} rcs $@ lht.o

lht_deps += ${BINDIR}/libmmf.a
lht_deps += ${BINDIR}/liblog.a
lht_deps += ${BINDIR}/liblht.a

${BINDIR}/lht: lht/lht-main.c ${lht_deps}
	${CC} -o $@ lht/lht-main.c ${CFLAGS} ${LFLAGS} -llog -lmmf -llht 
