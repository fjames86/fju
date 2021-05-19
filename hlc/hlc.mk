
hlc: hlc/hlc.c 

${BINDIR}/hlc.fvm: hlc/hlc.pas ${BINDIR}/fju 
	${BINDIR}/fju fvmc -o $@ -I fvm/stdlib hlc/hlc.pas

hlc/hlc.c: ${BINDIR}/hlc.fvm ${BINDIR}/fju 
	${BINDIR}/fju fvmc -o hlc/hlc.c -C ${BINDIR}/hlc.fvm 

FVMMODULES += hlc/hlc.c
