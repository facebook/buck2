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
#cgo CFLAGS: -std=c11
#cgo CXXFLAGS: -std=c++17
#include "lib.h"
*/
import "C"

import "fmt"

func main() {
	fmt.Printf("i = %d\n", int(C.c_part())+int(C.cxx_part()))
}
