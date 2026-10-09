/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

#include "lib.h"
#include <stdio.h>

// Deliberately no Foundation import: this asserts only that `go list` hands
// `.m` sources to the cxx toolchain and that the toolchain compiles them as
// Objective-C, without dragging in a link against the ObjC runtime.
#ifndef __OBJC__
#error "lib.m was not compiled as Objective-C"
#endif

void print_value(int i) {
  printf("i = %d\n", i);
  fflush(stdout);
}
