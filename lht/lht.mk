
lht: ${BINDIR}/lht ${LIBDIR}/liblht.a 

${LIBDIR}/liblht.a: lht/lht.c include/lht.h 
	${CC} -c $> ${CFLAGS} 
	${AR} rcs $@ lht.o

lht_deps += ${LIBDIR}/libmmf.a
lht_deps += ${LIBDIR}/liblog.a
lht_deps += ${LIBDIR}/liblht.a

${BINDIR}/lht: lht/lht-main.c ${lht_deps}
	${CC} -o $@ lht/lht-main.c ${CFLAGS} ${LFLAGS} -llog -lmmf -llht 
