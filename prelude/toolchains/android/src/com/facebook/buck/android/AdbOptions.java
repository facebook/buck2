/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.android;

public class AdbOptions {

  public static final String MULTI_INSTALL_MODE_SHORT_ARG = "-x";

  private String adbExecutablePath;
  private int adbThreadCount;
  private int adbServerPort;
  private boolean multiInstallMode;
  private boolean stagedInstallMode;
  private boolean ignoreMissingDevice;
  private boolean apexMode;
  private String restartMode;

  public AdbOptions(
      String adbExecutablePath,
      int adbThreadCount,
      int adbServerPort,
      boolean multiInstallMode,
      boolean stagedInstallMode,
      boolean ignoreMissingDevice,
      boolean apexMode,
      String restartMode) {
    this.adbExecutablePath = adbExecutablePath;
    this.adbThreadCount = adbThreadCount;
    this.adbServerPort = adbServerPort;
    this.multiInstallMode = multiInstallMode;
    this.stagedInstallMode = stagedInstallMode;
    this.ignoreMissingDevice = ignoreMissingDevice;
    this.apexMode = apexMode;
    this.restartMode = restartMode;
  }

  public String getAdbExecutablePath() {
    return adbExecutablePath;
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

  public boolean getIgnoreMissingDevice() {
    return ignoreMissingDevice;
  }

  public String getRestartMode() {
    return restartMode;
  }

  @Override
  public String toString() {
    return "AdbOptions{"
        + "adbExecutablePath="
        + adbExecutablePath
        + ", adbThreadCount="
        + adbThreadCount
        + ", adbServerPort="
        + adbServerPort
        + ", multiInstallMode="
        + multiInstallMode
        + ", stagedInstallMode="
        + stagedInstallMode
        + ", ignoreMissingDevice="
        + ignoreMissingDevice
        + ", apexMode="
        + apexMode
        + ", restartMode="
        + restartMode
        + '}';
  }
}
