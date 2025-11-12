/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.util;

import com.google.common.annotations.VisibleForTesting;

/**
 * A helper singleton that provides facilities such as extracting the native process id of a {@link
 * Process} or gathering the process resource consumption.
 */
public class ProcessHelper {

  private static final ProcessHelper INSTANCE = new ProcessHelper();

  /** Gets the singleton instance of this class. */
  public static ProcessHelper getInstance() {
    return INSTANCE;
  }

  /** This is a helper singleton. */
  @VisibleForTesting
  ProcessHelper() {}

  /**
   * @return whether the process has finished executing or not.
   */
  public boolean hasProcessFinished(Process process) {
    try {
      process.exitValue();
      return true;
    } catch (IllegalThreadStateException e) {
      return false;
    }
  }
}
