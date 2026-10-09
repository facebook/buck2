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

#if __STDC_VERSION__ != 201112L
#error "C source did not receive -std=c11 from the #cgo CFLAGS directive"
#endif

int c_part(void) {
  return 1;
}
