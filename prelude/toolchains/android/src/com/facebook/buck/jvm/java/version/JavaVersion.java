/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.java.version;

/** Enum that represents java version. */
public enum JavaVersion {
  VERSION_7("7"),
  VERSION_8("8"),
  VERSION_9("9"),
  VERSION_10("10"),
  VERSION_11("11"),
  VERSION_17("17"),
  VERSION_19("19"),
  VERSION_21("21"),
  VERSION_24("24");

  private final String version;

  JavaVersion(String version) {
    this.version = version;
  }

  public String getVersion() {
    return version;
  }

  /** Converts string java version into enum value of type {@link JavaVersion} */
  public static JavaVersion toJavaLanguageVersion(String version) {
    String versionString = version;
    double versionDouble = Double.parseDouble(version);
    if (versionDouble >= 1.5 && versionDouble <= 1.8) {
      versionString = Integer.toString(((int) (versionDouble * 10)) - 10);
    } else if (versionDouble % 1 == 0) { // if double doesn't have decimal part
      versionString = Integer.toString((int) versionDouble);
    }

    for (JavaVersion javaVersion : values()) {
      if (versionString.equals(javaVersion.getVersion())) {
        return javaVersion;
      }
    }
    throw new IllegalArgumentException("Can't find java version for string: " + version);
  }
}
