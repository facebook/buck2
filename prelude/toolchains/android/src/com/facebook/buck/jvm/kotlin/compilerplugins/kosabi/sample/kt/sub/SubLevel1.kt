/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.kotlin.compilerplugins.kosabi.sample.kt.sub

import com.facebook.kotlin.compilerplugins.kosabi.sample.kt.base.Sample

class SubLevel1 {
  fun bar(): Sample {
    return Sample()
  }

  // uncomment to change ABI
  // fun baxxxx(): Int {
  //  return 42
  // }
}
