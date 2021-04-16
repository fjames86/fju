
nls: ${BINDIR}/nls.fvm nls/nls.c

${BINDIR}/nls.fvm: ${BINDIR}/fju nls/nls.pas 
	${BINDIR}/fju fvmc -o $@ -I fvm/stdlib nls/nls.pas

nls/nls.c: ${BINDIR}/nls.fvm
	${BINDIR}/fju fvmc -o nls/nls.c -C ${BINDIR}/nls.fvm
