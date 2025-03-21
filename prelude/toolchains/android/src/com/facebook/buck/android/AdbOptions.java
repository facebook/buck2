/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android;

public class AdbOptions {

  public static final String MULTI_INSTALL_MODE_SHORT_ARG = "-x";

  private int adbThreadCount;
  private int adbServerPort;
  private boolean multiInstallMode;
  private boolean stagedInstallMode;
  private int adbTimeout;
  private boolean ignoreMissingDevice;
  private boolean apexMode;

  public AdbOptions(
      int adbThreadCount,
      int adbServerPort,
      boolean multiInstallMode,
      boolean stagedInstallMode,
      int adbTimeout,
      boolean ignoreMissingDevice,
      boolean apexMode) {
    this.adbThreadCount = adbThreadCount;
    this.adbServerPort = adbServerPort;
    this.multiInstallMode = multiInstallMode;
    this.stagedInstallMode = stagedInstallMode;
    this.adbTimeout = adbTimeout;
    this.ignoreMissingDevice = ignoreMissingDevice;
    this.apexMode = apexMode;
  }

  public int getAdbThreadCount() {
    return adbThreadCount;
  }

  public int getAdbServerPort() {
    return adbServerPort;
  }

  public boolean isMultiInstallModeEnabled() {
    return multiInstallMode;
  }

  public boolean isStagedInstallModeEnabled() {
    return stagedInstallMode;
  }

  public boolean isApexModeEnabled() {
    return apexMode;
  }

  public int getAdbTimeout() {
    return adbTimeout;
  }

  public boolean getIgnoreMissingDevice() {
    return ignoreMissingDevice;
  }
}
