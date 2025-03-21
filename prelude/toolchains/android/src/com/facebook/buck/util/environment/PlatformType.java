/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.util.environment;

/**
 * The type of the {@link Platform}, in only a very general sense: Windows, Unix, etc.
 *
 * <p>See boolean functions like {@link #isUnix()}, {@link #isWindows()}, ... for slightly easier
 * syntax: {@code if (Platform.detect().getType().isUnix()) ...}
 */
public enum PlatformType {
  UNKNOWN,
  UNIX,
  WINDOWS,
  ;

  /**
   * Whether this is {@link PlatformType#UNIX}. Makes for slightly easier syntax: {@code if
   * (Platform.detect().getType().isUnix()) { ... } }
   */
  public boolean isUnix() {
    return this == UNIX;
  }

  public boolean isWindows() {
    return this == WINDOWS;
  }
}
