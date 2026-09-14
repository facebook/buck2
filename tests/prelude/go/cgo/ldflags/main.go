/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package main

/*
#cgo darwin LDFLAGS: -framework CoreFoundation
#cgo linux LDFLAGS: -lm

// A space-bearing flag, which `go/build` permits (space is in its safeString)
// and which cgo re-splits with splitQuoted. Should the flags be concatenated
// rather than encoded, this arrives as two: `-L/nonexistent/dir` plus a stray
// `with`, and the link fails looking for a file by that name. The directory
// need not exist -- an unusable -L is merely a linker warning.
#cgo LDFLAGS: "-L/nonexistent/dir with space"
#include "lib.h"
*/
import "C"

import "fmt"

func main() {
	x := 16.0
	fmt.Printf("i = %d\n", int(C.four(C.double(x))))
}
