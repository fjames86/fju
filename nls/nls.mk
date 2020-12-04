
nls: ${BINDIR}/nls.fvm 

${BINDIR}/nls.fvm: ${BINDIR}/fvmc nls/nls.pas
	${BINDIR}/fvmc -o $@ fvm/stdlib/native.pas fvm/stdlib/xdr.pas nls/nls.pas
	rm nls/nls.asm
