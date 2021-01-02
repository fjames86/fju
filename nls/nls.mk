
nls: ${BINDIR}/nls.fvm 

${BINDIR}/nls.fvm: ${BINDIR}/fju nls/nls.pas 
	${BINDIR}/fju fvmc -o $@ -I fvm/stdlib nls/nls.pas
