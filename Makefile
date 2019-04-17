
.PHONY: all

all:
	cd mmf && $(MAKE) -f Makefile.bsd clean all 
	cd sec && $(MAKE) -f Makefile.bsd clean all
	cd dh && $(MAKE) -f Makefile.bsd clean all && cp ecdh ../bin 
	cd log && $(MAKE) -f Makefile.bsd clean all && cp fjlog ../bin 
	cd lht && $(MAKE) -f Makefile.bsd clean all && cp lht ../bin 
	cd hostreg && $(MAKE) -f Makefile.bsd clean all && cp hostreg ../bin 
	cd raft && $(MAKE) -f Makefile.bsd clean all && cp raft ../bin 
	cd rpc && $(MAKE) -f Makefile.bsd clean all && cp rpcd rpcinfo ../bin



