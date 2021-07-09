
/*
 * This provides an interface to the rpcd_main routine
 * from librpc. This runs the main event loop for fjud.
 */


package rpc

// #cgo CFLAGS: -I../../../include
// #cgo LDFLAGS: -L../../../lib -lrpc -lsec -lcrypto -lfreg -lftab -lmmf 
// #include "fju/rpc.h"
// #include "fju/rpcd.h"
/*
   extern void GoRpcd_MainCb(int evt);
   
   void gorpcd_maincb(rpcd_evt_t evt, void *arg, void *cxt ) {
       GoRpcd_MainCb((int)evt);
   }
 */
import "C"
import "unsafe"
import "os"

var g_evtcb func(int,interface{});
var g_evtcxt interface{}

const (
	EventInit = 0
	EventClose = 1
	EventSignal = 2
)

// RpcdMain run the event loop. evtcb is a callback invoked to indicate initialization etc 
func RpcdMain(evtcb func(int, interface{}), cxt interface{}) {
	g_evtcb = evtcb
	g_evtcxt = cxt
	
	argc := len(os.Args)
	var argv **C.char
	argv = (**C.char)(C.malloc(C.ulong(unsafe.Sizeof(*argv)) * C.ulong(argc)))
	defer C.free(unsafe.Pointer(argv))
	for i,p := range os.Args {
		pp := (**C.char)(unsafe.Pointer(uintptr(unsafe.Pointer(argv)) + uintptr(i)*uintptr(unsafe.Sizeof(*argv))))
		*pp = C.CString(p)
		defer C.free(unsafe.Pointer(*pp))
	}
	C.rpcd_main(C.int(argc),argv, (C.rpcd_main_t)(unsafe.Pointer(C.gorpcd_maincb)), nil)
}

// runEvtCb run the event callback 
func runEvtCb(evt int) {
	if g_evtcb != nil {
		g_evtcb( evt, g_evtcxt )
	}
}

