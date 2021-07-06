package log

// #cgo CFLAGS: -I../../../include
// #cgo LDFLAGS: -L../../../lib -llog 
// #include "fju/log.h"
import (
	"C"
	"fmt"
)

func logmsg(lvl int, fmt, args ...interface{}) {
	C.log_writef(nil, lvl, "%s", fmt.Sprintf(fmt, args...))
}

func Debug(fmt string, args ...interface{}) {
	logmsg(C.LOG_LVL_DEBUG, fmt, args)
}

func Error(fmt string, args ...interface{}) {
	logmsg(C.LOG_LVL_ERROR, fmt, args)
}

func Warn(fmt string, args ...interface{}) {
	logmsg(C.LOG_LVL_WARN, fmt, args)
}

func Trace(fmt string, args ...interface{}) {
	logmsg(C.LOG_LVL_TRACE, fmt, args)
}

