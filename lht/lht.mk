
lht: ${BINDIR}/lht.fvm lht/lht.c 

${BINDIR}/lht.fvm: ${BINDIR}/fju lht/lht.pas 
	${BINDIR}/fju fvmc -o $@ -I fvm/stdlib lht/lht.pas

lht/lht.c: ${BINDIR}/lht.fvm
	${BINDIR}/fju fvmc -o lht/lht.c -C ${BINDIR}/lht.fvm

FVMMODULES += lht/lht.c
