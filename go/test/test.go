
/*
 * This calls a routine from libfju library
 */

package main

import (
	"fmt"
	"fju"
)

func main() {
	fmt.Println("got here")
	fmt.Printf("fju randu32: %d\n", fju.Randu32())
	fmt.Println("end")
}
