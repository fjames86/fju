
nls: ${BINDIR}/nls.fvm 

${BINDIR}/nls.fvm: ${BINDIR}/fvmc nls/nls.pas
	${BINDIR}/fvmc -o $@ -I fvm/stdlib nls/nls.pas
