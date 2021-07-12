
/*
 * This calls a routine from libfju library
 */

package main

import (
	"fju/rpc"
	"net/http"
	"sync"
	"fju/log"
)

func main() {
	// register things
	go fjudHttpServer()

	// kick off rpcd service 
	log.Info("gfjud: go implementation of fjud starting...\n")
	rpc.RpcdMain(evtcb, nil)
	log.Info("gfjud stopped\n")
}

func evtcb(evt int, cxt interface{}) {
	switch evt {
	case rpc.EventInit:
		log.Info("gfjud started\n")

		// kick off any services, register rpc programs etc 
		fjuIterId = rpc.RegisterIterator(goChanIter, 1000)

		rpc.RegisterIterator(func () {
			log.Info("DUmmy iterator\n")
		}, 1000)
		
	case rpc.EventClose:
		log.Info("gfjud stopping\n")
	default:
	}
}

func fjudHttpServer() {
	http.HandleFunc("/fju", fjuHandler)
	http.ListenAndServe(":8090", nil)
}

var fjuIterId int

var fjuMsgLock sync.Mutex 
type fjuMsg struct {
	cb func(interface{})
	arg interface{}
}

var fjuMsgQueue = make([]fjuMsg, 0)

func fjuMsgPost(msg fjuMsg) {
	fjuMsgLock.Lock()
	fjuMsgQueue = append(fjuMsgQueue, msg)
	fjuMsgLock.Unlock()
}
func fjuMsgPop() (fjuMsg,bool) {
	var msg fjuMsg
	found := false
	
	fjuMsgLock.Lock()
	if len(fjuMsgQueue) > 0 {
		msg = fjuMsgQueue[0]
		fjuMsgQueue = fjuMsgQueue[1:]
		found = true
	}
	fjuMsgLock.Unlock()

	return msg, found
}

func fjuHandler(w http.ResponseWriter, req *http.Request) {
	log.Info("received request to /fju\n")
	FjuExecute(func(arg interface{}) {
		s := arg.(string)
		log.Info("%s\n", s)
	}, "my channel message")
}

// FjuExecuteAsync allows running a function in the context of fjud main thread
// It simply schedules the function to be run and returns immediately.
func FjuExecuteAsync(cb func(interface{}), arg interface{}) {
	msg := fjuMsg{ cb: cb, arg: arg }
	fjuMsgPost(msg)
	rpc.ScheduleIterator(fjuIterId)
}

// FjuExecute allows running a function in the context of fjud main thread
// and waiting for it to be completed. This function blocks until the function has returned.
func FjuExecute(cb func(interface{}), arg interface{}) {

	l := sync.Mutex{}
	cv := sync.NewCond(&l)
	
	FjuExecuteAsync(func (arg interface{}) {
		// execute the cb 
		cb(arg)

		// wake the calling thread 
		cv.Signal()
	}, arg)

	cv.L.Lock()
	cv.Wait()
	cv.L.Unlock()
}

// executes in context of rpcd thread 
func goChanIter() {
	log.Trace("goChanIter\n")
	msg, found := fjuMsgPop()
	if found && msg.cb != nil{
		msg.cb(msg.arg)
	}
}
