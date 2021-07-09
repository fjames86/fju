
package rpc

import "C"

//export GoRpcd_MainCb
func GoRpcd_MainCb(evt int) {
	runEvtCb(evt)
}


