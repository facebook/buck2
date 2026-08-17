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
      File apk,
      boolean installViaSd,
      boolean quiet,
      boolean stagedInstallMode,
      String packageName) {
    return installApkOnDevice(apk, installViaSd, quiet, true, stagedInstallMode, packageName);
  }

  default boolean installApkOnDevice(
      File apk,
      boolean installViaSd,
      boolean quiet,
      boolean verifyTempWritable,
      boolean stagedInstallMode,
      String packageName) {
    return installApkOnDevice(
        apk, installViaSd, quiet, verifyTempWritable, stagedInstallMode, null, packageName);
  }

  default boolean installApkOnDevice(
      File apk,
      boolean installViaSd,
      boolean quiet,
      boolean verifyTempWritable,
      boolean stagedInstallMode,
      @Nullable String userId,
      String packageName) {
    return installApkOnDevice(
        apk, installViaSd, quiet, verifyTempWritable, stagedInstallMode, userId, true, packageName);
  }

  /**
   * Install an APK on the device with optional user targeting, then confirm it actually landed.
   *
   * <p>After installing, the on-device apk is read back and compared to {@code apk}; if the package
   * is missing or the on-device apk is stale (which adb can report as success, notably under {@code
   * --fastdeploy}, where the real failure only reaches logcat), the install is retried without
   * {@code --fastdeploy}. The call fails if the apk still does not match. Staged installs are not
   * verified, since they are not applied until reboot.
   *
   * @param apk The APK file to install
   * @param installViaSd Whether to install via SD card
   * @param quiet If true, suppress output
   * @param verifyTempWritable If true, verify temp folder is writable before install
   * @param stagedInstallMode If true, use staged installation
   * @param userId User to install for: "all" for all users, a specific user ID (e.g., "10"), or
   *     null for default behavior (current user only)
   * @param allowFastDeploy If true, this install may use `adb install --fastdeploy` on supported
   *     devices.
   * @param packageName The package name of {@code apk}, used to read the installed apk back and
   *     verify the install took effect.
   * @return true if installation succeeded
   */
  boolean installApkOnDevice(
      File apk,
      boolean installViaSd,
      boolean quiet,
      boolean verifyTempWritable,
      boolean stagedInstallMode,
      @Nullable String userId,
      boolean allowFastDeploy,
      String packageName);

  default boolean installApexOnDevice(File apex, boolean quiet) {
    return installApexOnDevice(apex, quiet, true);
  }

  default boolean installApexOnDevice(File apex, boolean quiet, boolean restart) {
    return installApexOnDevice(apex, quiet, restart, false);
  }

  default boolean installApexOnDevice(
      File apex, boolean quiet, boolean restart, boolean waitForDeviceReady) {
    boolean softRebootAvailable = prepareForApexInstallation();
    return installApexOnDevice(apex, quiet, restart, softRebootAvailable, waitForDeviceReady);
  }

  boolean installApexOnDevice(
      File apex,
      boolean quiet,
      boolean restart,
      boolean softRebootAvailable,
      boolean waitForDeviceReady);

  boolean prepareForApexInstallation();

  void stopPackage(String packageName) throws Exception;

  Optional<PackageInfo> getPackageInfo(String packageName) throws Exception;

  void uninstallPackage(String packageName) throws Exception;

  String getApkManifestDigest(String packagePath) throws Exception;

  String getContentHash(String path) throws Exception;

  ImmutableSortedSet<Path> listDirRecursive(Path dirPath) throws Exception;

  void rmFiles(String dirPath, Iterable<String> filesToDelete);

  /**
   * Removes scratch this device left behind while installing {@code packageName}, including whole
   * payloads orphaned by an install interrupted mid-transfer. Does not touch installed files.
   */
  void rmStaleFiles(String packageName) throws Exception;

  AutoCloseable createForward() throws Exception;

  /**
   * Pushes {@code installPaths}, a map of device destination to local source.
   *
   * <p>Called concurrently for the same package: an install pushes its payload as several shards at
   * once. Whatever an implementation stages under has to be unique per call, or concurrent shards
   * overwrite each other on the way in.
   *
   * @param packageName the app these files belong to. Scopes whatever scratch the device needs, so
   *     that installs of different apps cannot disturb each other's transfers.
   */
  void installFiles(String filesType, Map<Path, Path> installPaths, String packageName)
      throws Exception;

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

  /**
   * Size, used and available space on the data partition, or {@code "_"} for each if it cannot be
   * read.
   *
   * @param humanReadable values suffixed for display, e.g. {@code 17G}. Otherwise they are plain
   *     counts of 1K blocks, which is what arithmetic wants.
   */
  default List<String> getDiskSpace(boolean humanReadable) {
    return Arrays.asList("_", "_", "_");
  }

  default void fixRootDir(String rootDir) {}

  boolean setDebugAppPackageName(@Nullable String packageName) throws Exception;

  void enableAppLinks(@Nullable String packageName) throws Exception;
}
