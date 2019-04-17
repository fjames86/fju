
all: rpcd rpcinfo librpc.a 

rpcd: librpc.a rpcd-main.c 
	gcc -o rpcd -g rpcd-main.c -lcrypto -L. -lrpc 

rpcinfo: rpcinfo.c librpc.a 
	gcc -o rpcinfo -g rpcinfo.c -lcrypto -L. -lrpc 

librpc.a: rpc.c rpcd.c rpc.h rpcd.h shauth.c shauth.h 
	gcc -c rpc.c rpcd.c shauth.c 
	ar rcs librpc.a rpc.o rpcd.o shauth.o 

.PHONY: tar clean all 
tar:
	tar -czvf rpc.tar.gz rpc.h rpc.c Makefile rpcd.c rpcinfo.c shauth.h shauth.c rpcd-main.c 

clean:
	rm rpcinfo rpcd rpc.o rpcd.o librpc.a shauth.o 
