/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android;

import com.facebook.buck.core.util.immutables.BuckStyleValue;

/** Information gathered from a device during buck install. */
@BuckStyleValue
public interface AndroidDeviceInfo {
  int TVDPI_DPI = 213;

  /** The display density category of the device. */
  enum DensityClass {
    LDPI(120),
    MDPI(160),
    HDPI(240),
    XHDPI(320),
    XXHDPI(480),
    XXXHDPI(640),
    OTHER_DPI(-1),
    TVDPI(-1);

    private final int maxDotsPerInch;

    DensityClass(int maxDotsPerInch) {
      this.maxDotsPerInch = maxDotsPerInch;
    }

    static DensityClass forPhysicalDensity(String dpiString) {
      try {
        int dpi = Integer.parseInt(dpiString);
        if (dpi == TVDPI_DPI) {
          return TVDPI;
        }
        for (DensityClass d : values()) {
          if (dpi <= d.maxDotsPerInch) {
            return d;
          }
        }
        return OTHER_DPI;
      } catch (NumberFormatException e) {
        return OTHER_DPI;
      }
    }
  }

  String getLocale();

  String getAbi();

  String getBuildFingerprint();

  DensityClass getDensity();

  String getDpi();

  String getSdk();

  boolean isEmulator();

  String getAndroidDeviceImplementation();

  static AndroidDeviceInfo of(
      String locale,
      String abi,
      String buildFingerprint,
      String dotsPerInch,
      String sdk,
      boolean isEmulator,
      String deviceImpl) {
    return ImmutableAndroidDeviceInfo.ofImpl(
        locale,
        abi,
        buildFingerprint,
        DensityClass.forPhysicalDensity(dotsPerInch),
        dotsPerInch,
        sdk,
        isEmulator,
        deviceImpl);
  }
}
