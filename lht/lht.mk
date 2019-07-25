
.PHONY: liblht

lht: ${BINDIR}/lht liblht

liblht: ${BINDIR}/liblht.a 

${BINDIR}/liblht.a: lht/lht.c 
	${CC} -c $> ${CFLAGS} 
	${AR} rcs $@ lht.o

${BINDIR}/lht: lht/lht-main.c mmf liblht liblog libmmf 
	${CC} -o $@ lht/lht-main.c ${CFLAGS} ${LFLAGS} -llog -lmmf -llht 
