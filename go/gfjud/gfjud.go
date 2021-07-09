
/*
 * This calls a routine from libfju library
 */

package main

import (
	"fmt"
	"fju/rpc"
	"net/http"
)

func main() {
	// register things
	go fjudHttpServer()

	// kick off rpcd service 
	fmt.Printf("gfjud: go implementation of fjud starting...\n")
	rpc.RpcdMain(evtcb, nil)
	fmt.Printf("gfjud stopped\n")
}

func evtcb(evt int, cxt interface{}) {
	switch evt {
	case rpc.EventInit:
		fmt.Printf("gfjud started\n")

		// kick off any services, register rpc programs etc 
		fjuIterId = rpc.RegisterIterator(goChanIter, 1000)

		rpc.RegisterIterator(func () {
			fmt.Printf("DUmmy iterator\n")
		}, 1000)
		
	case rpc.EventClose:
		fmt.Printf("gfjud stopping\n")
	default:
	}
}

func fjudHttpServer() {
	http.HandleFunc("/fju", fjuHandler)
	http.ListenAndServe(":8090", nil)
}

var fjuIterId int

type fjuMsg interface{}
var fjuMsgQueue = make([]fjuMsg, 0)

func fjuMsgPost(msg fjuMsg) {
	fjuMsgQueue = append(fjuMsgQueue, msg)
}
func fjuMsgPop() fjuMsg {
	if len(fjuMsgQueue) == 0 {
		return nil
	}
	
	msg := fjuMsgQueue[0]
	fjuMsgQueue = fjuMsgQueue[1:]
	return msg
}

func fjuHandler(w http.ResponseWriter, req *http.Request) {
	fmt.Printf("received request to /fju\n")
	FjuChannelPost("my channel message")
}

func FjuChannelPost(val interface{}) {
	fjuMsgPost((fjuMsg)(val))
	rpc.ScheduleIterator(fjuIterId)
}

func goChanIter() {
	fmt.Printf("goChanIter\n")
	value := fjuMsgPop()
	if value != nil {
		fmt.Printf("goChanIter got a value: %v", value)
	}
}
