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

import com.google.common.collect.ImmutableSortedSet;
import java.io.File;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;

public interface AndroidDevice {
  default boolean installApkOnDevice(
      File apk, boolean installViaSd, boolean quiet, boolean stagedInstallMode) {
    return installApkOnDevice(apk, installViaSd, quiet, true, stagedInstallMode);
  }

  boolean installApkOnDevice(
      File apk,
      boolean installViaSd,
      boolean quiet,
      boolean verifyTempWritable,
      boolean stagedInstallMode);

  boolean installApexOnDevice(File apex, boolean quiet);

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

  String deviceStartIntent(AndroidIntent intent) throws Exception;

  boolean uninstallApkFromDevice(String packageName, boolean keepData) throws Exception;

  String getInstallerMethodName();

  default List<String> getDiskSpace() {
    return Arrays.asList("_", "_", "_");
  }

  default void fixRootDir(String rootDir) {}

  boolean setDebugAppPackageName(String packageName) throws Exception;
}
