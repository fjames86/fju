
.PHONY: all

all:
	cd mmf && $(MAKE) -f Makefile.bsd clean 
	cd sec && $(MAKE) -f Makefile.bsd clean 
	cd dh && $(MAKE) -f Makefile.bsd clean 
	cd log && $(MAKE) -f Makefile.bsd clean
	cd lht && $(MAKE) -f Makefile.bsd clean
	cd hostreg && $(MAKE) -f Makefile.bsd clean 
	cd raft && $(MAKE) -f Makefile.bsd clean 
	cd rpc && $(MAKE) -f Makefile.bsd clean 
	cd mmf && $(MAKE) -f Makefile.bsd all 
	cd sec && $(MAKE) -f Makefile.bsd all
	cd dh && $(MAKE) -f Makefile.bsd all && cp ecdh ../bin 
	cd log && $(MAKE) -f Makefile.bsd all && cp fjlog ../bin 
	cd lht && $(MAKE) -f Makefile.bsd all && cp lht ../bin 
	cd hostreg && $(MAKE) -f Makefile.bsd libhostreg.a libhrauth.a 
	cd raft && $(MAKE) -f Makefile.bsd libraft.a
	cd nls && $(MAKE) -f Makefile libnls.a  
	cd rpc && $(MAKE) -f Makefile.bsd all && cp rpcd rpcinfo ../bin
	cd raft && $(MAKE) -f Makefile.bsd all && cp raft ../bin
	cd nls && $(MAKE) -f Makefile nls && cp nls ../bin 
	cd hostreg && $(MAKE) -f Makefile.bsd all && cp hostreg ../bin 
	scp bin/* root@169.254.54.129:~/bin 




