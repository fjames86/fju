
mmf: ${BINDIR}/libmmf.a

mmf_src += mmf/mmf.c 

${BINDIR}/libmmf.a: ${mmf_src}
	${CC} -c $> ${CFLAGS} 
	${AR} rcs $@ mmf.o

