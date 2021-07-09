
/*
 * This provides a go function to be called from C code to run the go
 * main event callback. It is exported so it can be called from C - see the header
 * function in rpcd.go 
 */
package rpc

import "C"
import "fmt"

//export GoRpcd_MainCb
func GoRpcd_MainCb(evt int) {
	runEvtCb(evt)
}


//export GoRpcIterCb
func GoRpcIterCb(id int) {
	fmt.Printf("GoRpcIterCb\n")
	cb := iterCbById(id)
	if cb != nil {
		cb()
	}
}
