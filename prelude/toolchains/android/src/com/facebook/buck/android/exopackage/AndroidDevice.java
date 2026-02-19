/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.android.exopackage;

import com.facebook.infer.annotation.Nullsafe;
import com.google.common.collect.ImmutableSortedSet;
import java.io.File;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.jetbrains.annotations.Nullable;

@Nullsafe(Nullsafe.Mode.LOCAL)
public interface AndroidDevice {
  default boolean installApkOnDevice(
      File apk, boolean installViaSd, boolean quiet, boolean stagedInstallMode) {
    return installApkOnDevice(apk, installViaSd, quiet, true, stagedInstallMode);
  }

  default boolean installApkOnDevice(
      File apk,
      boolean installViaSd,
      boolean quiet,
      boolean verifyTempWritable,
      boolean stagedInstallMode) {
    return installApkOnDevice(
        apk, installViaSd, quiet, verifyTempWritable, stagedInstallMode, null);
  }

  /**
   * Install an APK on the device with optional user targeting.
   *
   * @param apk The APK file to install
   * @param installViaSd Whether to install via SD card
   * @param quiet If true, suppress output
   * @param verifyTempWritable If true, verify temp folder is writable before install
   * @param stagedInstallMode If true, use staged installation
   * @param userId User to install for: "all" for all users, a specific user ID (e.g., "10"), or
   *     null for default behavior (current user only)
   * @return true if installation succeeded
   */
  boolean installApkOnDevice(
      File apk,
      boolean installViaSd,
      boolean quiet,
      boolean verifyTempWritable,
      boolean stagedInstallMode,
      @Nullable String userId);

  default boolean installApexOnDevice(File apex, boolean quiet) {
    return installApexOnDevice(apex, quiet, true);
  }

  default boolean installApexOnDevice(File apex, boolean quiet, boolean restart) {
    boolean softRebootAvailable = prepareForApexInstallation();
    return installApexOnDevice(apex, quiet, restart, softRebootAvailable);
  }

  boolean installApexOnDevice(
      File apex, boolean quiet, boolean restart, boolean softRebootAvailable);

  boolean prepareForApexInstallation();

  void stopPackage(String packageName) throws Exception;

  Optional<PackageInfo> getPackageInfo(String packageName) throws Exception;

  void uninstallPackage(String packageName) throws Exception;

  String getSignature(String packagePath) throws Exception;

  ImmutableSortedSet<Path> listDirRecursive(Path dirPath) throws Exception;

  void rmFiles(String dirPath, Iterable<String> filesToDelete);

  AutoCloseable createForward() throws Exception;

  void installFiles(String filesType, Map<Path, Path> installPaths) throws Exception;

  void mkDirP(String dirpath) throws Exception;

  String getProperty(String name) throws Exception;

  List<String> getDeviceAbis() throws Exception;

  void killProcess(String processName) throws Exception;

  String getSerialNumber();

  String getWindowManagerProperty(String propertyName) throws Exception;

  boolean isEmulator();

  boolean isOnline();

  boolean installBuildUuidFile(Path dataRoot, String packageName, String buildUuid)
      throws Exception;

  String deviceStartIntent(@Nullable AndroidIntent intent) throws Exception;

  boolean uninstallApkFromDevice(String packageName, boolean keepData) throws Exception;

  String getInstallerMethodName();

  default List<String> getDiskSpace() {
    return Arrays.asList("_", "_", "_");
  }

  default void fixRootDir(String rootDir) {}

  boolean setDebugAppPackageName(@Nullable String packageName) throws Exception;

  void enableAppLinks(@Nullable String packageName) throws Exception;
}
