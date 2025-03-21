/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.kotlin.compilerplugins.kosabi.sample.java.sub;

import com.facebook.infer.annotation.Nullsafe;
import com.facebook.kotlin.compilerplugins.kosabi.sample.java.base.Sample;

@Nullsafe(Nullsafe.Mode.LOCAL)
public class SubSample {
  public Sample bar() {
    return new Sample();
  }
}
