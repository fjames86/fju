
/*
 * This provides go interface to libfju codes
 * at the moment it is dynamically linking against libfju.so
 * but we could (should?) change to statically link against individual libraries (libraft.a etc)
 */

package fju

// #cgo CFLAGS: -I../../include
// #cgo LDFLAGS: -L../../lib -lsec -lcrypto
// #include "fju/sec.h"
import "C"

func Randu32() uint32 {
	return uint32(C.sec_rand_uint32())
}



