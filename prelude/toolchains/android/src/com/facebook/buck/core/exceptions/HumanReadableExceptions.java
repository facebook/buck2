/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.core.exceptions;

/**
 * Utilities to work with {@link HumanReadableException} and {@link
 * ExceptionWithHumanReadableMessage}.
 */
public class HumanReadableExceptions {

  private HumanReadableExceptions() {}

  /**
   * Rethrow exception if it is runtime exception and if it implements {@link
   * ExceptionWithHumanReadableMessage}.
   */
  public static void throwIfHumanReadableUnchecked(Throwable e) {
    if (e instanceof RuntimeException && e instanceof ExceptionWithHumanReadableMessage) {
      throw (RuntimeException) e;
    }
  }
}
