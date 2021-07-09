
/*
 * This calls a routine from libfju library
 */

package main

import (
	"fmt"
	"fju"
	"fju/log"
	"fju/xdr"
	"fju/rpc"
)

func main() {
	fmt.Println("got here")
	fmt.Printf("fju randu32: %d\n", fju.Randu32())

	log.Debug("hello from go")

	buf := make([]byte, 1024)
	xdr := xdr.New(buf)
	xdr.EncodeString("hello")
	for i := 0; i < xdr.Offset(); i++ {
		fmt.Printf( "%x ", buf[i] )
	}
	fmt.Printf("\n")
	for _, x := range xdr.Buffer() {
		fmt.Printf( "%x ", x )
	}
	fmt.Printf( "\n")

	pars := rpc.CallPars {
		Prog: 100000,
			Vers : 2,
			Proc : 4,
			Timeout : 1000,
		}
	args := make([]byte, 0)
	resbuf := rpc.CallUdp(pars, args)
	if resbuf == nil {
		fmt.Println("CallUdp failed")
	} else {
		fmt.Println("CallUDp result %v", resbuf)
	}


	rpc.RpcdMain(evtcb, nil)
	fmt.Println("end")
}

func evtcb(evt int, cxt interface{}) {
	var msg string
	
	switch evt {
	case rpc.EventInit:
		msg = "Init"
	case rpc.EventClose:
		msg = "Close"
	default:
	}
	
	fmt.Printf("got to evtcb with event %s\n", msg)
}
