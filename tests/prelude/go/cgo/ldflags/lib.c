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

#ifdef __APPLE__
#include <CoreFoundation/CoreFoundation.h>
#else
#include <math.h>
#endif

// Both branches return 4, by way of a library that only the matching
// `#cgo LDFLAGS:` directive in main.go puts on the link line. Drop those
// directives and this fails to link rather than failing to run.
int four(double x) {
#ifdef __APPLE__
  (void)x;
  CFStringRef s = CFStringCreateWithCString(NULL, "abcd", kCFStringEncodingUTF8);
  int n = (int)CFStringGetLength(s);
  CFRelease(s);
  return n;
#else
  // A runtime argument, so the compiler cannot fold the call away and leave
  // the link with nothing to resolve.
  return (int)sqrt(x);
#endif
}
