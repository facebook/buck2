/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.testrunner;

import com.android.ddmlib.IDevice;
import com.facebook.buck.testrunner.reportlayer.TombstonesReportLayer;
import com.facebook.buck.testrunner.reportlayer.VideoRecordingReportLayer;
import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Map;

public class InstrumentationTestRunnerForClout extends InstrumentationTestRunner {

  private static final String ANDROID_HOME = "ANDROID_HOME";
  private static final String CLOUT_ORIG_ADB_PATH = "/platform-tools/adb.orig";

  public InstrumentationTestRunnerForClout(
      DeviceArgs deviceArgs,
      String packageName,
      String targetPackageName,
      String testRunner,
      File outputDirectory,
      String instrumentationApkPath,
      String apkUnderTestPath,
      String exopackageLocalPath,
      String apkUnderTestExopackageLocalPath,
      boolean attemptUninstallApkUnderTest,
      boolean attemptUninstallInstrumentationApk,
      boolean debug,
      boolean codeCoverage,
      String codeCoverageOutputFile,
      boolean isSelfInstrumenting,
      Map<String, String> extraInstrumentationArguments,
      String extraInstrumentationTestListener,
      Map<String, String> extraFilesToPull,
      Map<String, String> extraDirsToPull,
      boolean clearPackageData,
      boolean disableAnimations,
      String preTestSetupScript,
      List<String> extraApksToInstall) {
    super(
        deviceArgs,
        packageName,
        targetPackageName,
        testRunner,
        outputDirectory,
        instrumentationApkPath,
        apkUnderTestPath,
        exopackageLocalPath,
        apkUnderTestExopackageLocalPath,
        attemptUninstallApkUnderTest,
        attemptUninstallInstrumentationApk,
        debug,
        codeCoverage,
        codeCoverageOutputFile,
        isSelfInstrumenting,
        extraInstrumentationArguments,
        extraInstrumentationTestListener,
        extraFilesToPull,
        extraDirsToPull,
        clearPackageData,
        disableAnimations,
        preTestSetupScript,
        extraApksToInstall);
  }

  @SuppressWarnings("PMD.BlacklistedSystemGetenv")
  public static InstrumentationTestRunnerForClout fromArgs(String... args) throws IOException {
    ArgsParser argsParser = new ArgsParser();
    argsParser.fromArgs(args);
    DeviceArgs deviceArgs = getDeviceArgs(args);
    InstrumentationTestRunnerForClout runner =
        new InstrumentationTestRunnerForClout(
            deviceArgs,
            argsParser.packageName,
            argsParser.targetPackageName,
            argsParser.testRunner,
            argsParser.outputDirectory,
            argsParser.instrumentationApkPath,
            argsParser.apkUnderTestPath,
            argsParser.exopackageLocalPath,
            argsParser.apkUnderTestExopackageLocalPath,
            argsParser.attemptUninstallApkUnderTest,
            argsParser.attemptUninstallInstrumentationApk,
            argsParser.debug,
            argsParser.codeCoverage,
            argsParser.codeCoverageOutputFile,
            argsParser.isSelfInstrumenting,
            argsParser.extraInstrumentationArguments,
            argsParser.extraInstrumentationTestListener,
            argsParser.extraFilesToPull,
            argsParser.extraDirsToPull,
            argsParser.clearPackageData,
            argsParser.disableAnimations,
            argsParser.preTestSetupScript,
            argsParser.extraApksToInstall);
    if (argsParser.recordVideo) {
      runner.addReportLayer(new VideoRecordingReportLayer(runner));
    }
    if (argsParser.collectTombstones) {
      runner.addReportLayer(new TombstonesReportLayer(runner));
    }
    return runner;
  }

  @SuppressWarnings("PMD.BlacklistedSystemGetenv")
  private String getAndroidHome() {
    if (System.getenv().containsKey(ANDROID_HOME)) {
      return System.getenv(ANDROID_HOME);
    } else {
      return null;
    }
  }

  @Override
  protected String getAdbPath() {
    String androidHome = getAndroidHome();
    if (androidHome != null) {
      // clout overrwrote adb to some custom script, this script hangs test runner.
      // we are reverting it therefore to the original adb binary for test runner.
      return androidHome + CLOUT_ORIG_ADB_PATH;
    } else {
      throw new RuntimeException("Could not find ANDROID_HOME env variable");
    }
  }

  @Override
  protected void installPackage(IDevice device, String path) throws Throwable {
    RunShellCommand.run(getAdbPath(), "install " + path);
  }
}
