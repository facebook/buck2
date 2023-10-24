/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#include <zlib.h>
#include <cstring>
#include <iostream>

int main() {
  uLong adler = adler32(0L, Z_NULL, 0);

  const char msg[] = "Hello Conan";
  adler = adler32(adler, reinterpret_cast<const Bytef*>(msg), strlen(msg));

  std::cout << adler << std::endl;
}
