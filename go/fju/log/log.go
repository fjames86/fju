package log

// #cgo CFLAGS: -I../../../include
// #cgo LDFLAGS: -L../../../lib -llog -lmmf 
// #include "fju/log.h"
/*
   int log_writemsg_go(int lvl, char *msg) {
       return log_writef( NULL, lvl, "%s", msg );
   }
*/
import "C"

import (
	"fmt"
)

const (
	lvlTrace = iota
	lvlDebug
	lvlInfo
	lvlWarn
	lvlError
	lvlFatal
)

func logmsg(lvl int, f string, args ...interface{}) {
	C.log_writemsg_go(C.int(lvl), C._GoStringPtr(fmt.Sprintf(f, args...)))
}

func Debug(fmt string, args ...interface{}) {
	logmsg(lvlDebug, fmt, args)
}

func Error(fmt string, args ...interface{}) {
	logmsg(lvlError, fmt, args)
}

func Warn(fmt string, args ...interface{}) {
	logmsg(lvlWarn, fmt, args)
}

func Trace(fmt string, args ...interface{}) {
	logmsg(lvlTrace, fmt, args)
}

func Info(fmt string, args ...interface{}) {
	logmsg(lvlInfo, fmt, args)
}

