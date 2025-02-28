/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.util.zip;

public enum ZipCompressionLevel {
  NONE(0),
  MIN(1),
  DEFAULT(6),
  MAX(9),
  ;

  private final int value;

  ZipCompressionLevel(int value) {
    this.value = value;
  }

  public int getValue() {
    return value;
  }
}
