
package rpc

// #cgo CFLAGS: -I../../../include
// #cgo LDFLAGS: -L../../../lib -lrpc -lsec -lcrypto
// #include "fju/rpc.h"
/*
   void rpc_callpars_setaddr(struct rpc_call_pars *pars, int ip, int port) {
   struct sockaddr_in *sinp;
   sinp = (struct sockaddr_in *)&pars->raddr;
   sinp->sin_port = htons( port );
   sinp->sin_addr.s_addr = ip;
   pars->raddr_len = sizeof(*sinp);
   sinp->sin_family = AF_INET;
   sinp->sin_len = sizeof(*sinp);
   }

   extern void GoRpcIterCb(int id);
   void gorpc_iter_cb( struct rpc_iterator *iter ) {
       // call back into go
       GoRpcIterCb((int)iter->cxt);
   }
 */
import "C"
import "unsafe"
import "net"

func Now() uint64 {
	return uint64(C.rpc_now())
}

type CallPars struct {
	Prog uint32
	Vers uint32
	Proc uint32
	Timeout int
}

func CallUdp(pars CallPars, args []byte) []byte {
	var p *C.uchar
	if len(args) > 0 {
		p = (*C.uchar)(unsafe.Pointer(&args[0]))
	} else {
		p = (*C.uchar)(nil)
	}

	xdr := (*C.struct_xdr_s)(C.malloc(C.ulong(unsafe.Sizeof(new(C.struct_xdr_s)))))
	defer C.free(unsafe.Pointer(xdr))
	xdr.buf = p
	xdr.offset = 0
	xdr.count = C.int(len(args))

	resxdr := (*C.struct_xdr_s)(C.malloc(C.ulong(unsafe.Sizeof(new(C.struct_xdr_s)))))
	defer C.free(unsafe.Pointer(resxdr))
	
	ppars := C.struct_rpc_call_pars {
		prog: C.uint(pars.Prog),
			vers: C.uint(pars.Vers),
			proc: C.uint(pars.Proc),
			timeout: C.int(pars.Timeout),
		}

	ipaddr := net.ParseIP("127.0.0.1")
	ipa := (uint(ipaddr[0]) << 24) | (uint(ipaddr[1]) << 16) | (uint(ipaddr[2]) << 8) | uint(ipaddr[3])
	
	C.rpc_callpars_setaddr( &ppars, C.int(ipa), 8000 );
	ppars.buf.buf = (*C.uchar)(C.malloc(4096))
	defer C.free(unsafe.Pointer(ppars.buf.buf))
	ppars.buf.count = 4096
	
	sts := C.rpc_call_udp( &ppars, xdr, resxdr )
	if sts == -1 {
		return nil
	}

	resbuf := make([]byte, resxdr.count - resxdr.offset)
	C.memcpy(unsafe.Pointer(&resbuf[0]), unsafe.Pointer(uintptr(unsafe.Pointer(resxdr.buf)) + uintptr(resxdr.offset)), C.ulong(resxdr.count - resxdr.offset))
	return resbuf
}

type iterEntry struct {
	id int
	cb func()
	ptr *C.struct_rpc_iterator
}

var iterid = 0 
var itermap = make(map[int]iterEntry)

func iterCbById(id int) func() {
	e, ok := itermap[id]
	if ok {
		return e.cb
	}
	return nil
}

func ScheduleIterator(id int) {
	entry, ok := itermap[id]
	if ok {
		entry.ptr.timeout = 0
	}
}

func RegisterIterator(cb func(), period int) int {
	var iter *C.struct_rpc_iterator
	iter = (*C.struct_rpc_iterator)(C.malloc(C.ulong(unsafe.Sizeof(*iter))))
	iter.next = nil
	iter.timeout = 0
	iter.period = C.int(period)
	iter.cb = (C.rpc_iterator_t)(unsafe.Pointer(C.gorpc_iter_cb))
	iter.cxt = unsafe.Pointer((uintptr)(iterid))

	entry := new(iterEntry)
	entry.id = iterid
	entry.cb = cb
	entry.ptr = iter
	itermap[iterid] = *entry
	iterid += 1
	
	C.rpc_iterator_register(iter)
	return entry.id
}


