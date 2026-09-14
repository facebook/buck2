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

// Two failures are caught here. If CXXFLAGS are dropped, this fires. If CFLAGS
// are instead applied to C++ sources -- as they were before they were split by
// language -- the compiler rejects `-std=c11` outright on this file.
#if __cplusplus != 201703L
#error "C++ source did not receive -std=c++17 from the #cgo CXXFLAGS directive"
#endif

#include <string>

int cxx_part(void) {
  return static_cast<int>(std::string("abc").size());
}
