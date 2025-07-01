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

import com.facebook.buck.core.util.log.Logger;
import com.google.common.annotations.VisibleForTesting;
import com.zaxxer.nuprocess.NuProcess;
import javax.lang.model.SourceVersion;

/**
 * A helper singleton that provides facilities such as extracting the native process id of a {@link
 * Process} or gathering the process resource consumption.
 */
public class ProcessHelper {

  private static final Logger LOG = Logger.get(ProcessHelper.class);

  // Comparing with the string value to avoid a strong dependency on JDK 9+
  // TODO: Remove the version checks once Buck has been migrated to Java 10/11.
  private static final boolean IS_JDK9_OR_LATER =
      SourceVersion.latest().toString().equals("RELEASE_9")
          || SourceVersion.latest().toString().equals("RELEASE_10")
          || SourceVersion.latest().toString().equals("RELEASE_11");

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
  public boolean hasProcessFinished(Object process) {
    if (process instanceof NuProcess) {
      return !((NuProcess) process).isRunning();
    } else if (process instanceof Process) {
      try {
        ((Process) process).exitValue();
        return true;
      } catch (IllegalThreadStateException e) {
        return false;
      }
    } else {
      throw new IllegalArgumentException("Unknown process class: " + process.getClass());
    }
  }
}
