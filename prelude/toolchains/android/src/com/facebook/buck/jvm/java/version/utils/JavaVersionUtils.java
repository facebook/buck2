/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java.version.utils;

import java.util.Objects;

/** Utility class for retrieving Java version information. */
public class JavaVersionUtils {
  private static int majorVersion = 0;

  /**
   * Returns the major version of the current JVM instance (e.g. 8 for Java 1.8, and 10 for Java
   * 10.0).
   */
  public static int getMajorVersion() {
    if (majorVersion == 0) {
      majorVersion =
          getMajorVersionFromString(Objects.requireNonNull(System.getProperty("java.version")));
    }
    return majorVersion;
  }

  /** Returns the major version from a Java version string (e.g. 8 for "1.8", and 10 for "10.0"). */
  public static int getMajorVersionFromString(String version) {
    String[] versionParts = Objects.requireNonNull(version).split("\\.");
    return Integer.parseInt((versionParts[0].equals("1")) ? versionParts[1] : versionParts[0]);
  }
}
