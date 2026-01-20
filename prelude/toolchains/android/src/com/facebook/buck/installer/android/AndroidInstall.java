/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.installer.android;

import static java.util.Map.entry;

import com.facebook.buck.android.AdbExecutionContext;
import com.facebook.buck.android.AdbHelper;
import com.facebook.buck.android.AdbOptions;
import com.facebook.buck.android.IsolatedApkInfo;
import com.facebook.buck.android.device.TargetDeviceOptions;
import com.facebook.buck.android.exopackage.AdbUtils;
import com.facebook.buck.android.exopackage.AndroidDeviceInfo;
import com.facebook.buck.android.exopackage.IsolatedExopackageInfo;
import com.facebook.buck.android.exopackage.SetDebugAppMode;
import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.installer.InstallId;
import com.facebook.buck.installer.InstallResult;
import com.facebook.buck.util.Console;
import com.facebook.buck.util.Verbosity;
import com.google.common.io.ByteStreams;
import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger; // NOPMD

/** Installs an Android Apk */
class AndroidInstall {
  private static final Logger LOG = Logger.getLogger(AndroidInstall.class.getName());
  private static final Set<String> ENABLE_APP_LINKS_ALLOWLIST =
      Set.of("com.facebook.wakizashi", "com.facebook.lite", "com.instagram.lite");

  private final IsolatedApkInfo apkInfo;
  private final Optional<IsolatedExopackageInfo> exopackageInfo;
  private final AbsPath rootPath;
  private final InstallId installId;
  private final AndroidCommandLineOptions cliOptions;
  private final boolean installViaSd = false;
  private final Logger logger;
  private final AdbHelper adbHelper;
  private final ByteArrayOutputStream stderr;

  public AndroidInstall(
      Logger logger,
      AbsPath rootPath,
      AndroidCommandLineOptions cliOptions,
      AndroidInstallApkOptions apkOptions,
      IsolatedApkInfo apkInfo,
      Optional<IsolatedExopackageInfo> exopackageInfo,
      InstallId installId) {
    this.logger = logger;
    this.rootPath = rootPath;
    this.apkInfo = apkInfo;
    this.exopackageInfo = exopackageInfo;
    this.installId = installId;
    this.cliOptions = cliOptions;

    // Set-up adbOptions
    AdbOptions adbOptions =
        new AdbOptions(
            cliOptions.adbExecutablePath,
            cliOptions.adbThreadCount,
            cliOptions.adbServerPort,
            cliOptions.multiInstallMode,
            apkOptions.stagedInstallMode,
            cliOptions.ignoreMissingDevices,
            apkOptions.apexMode,
            cliOptions.restartMode.name());
    LOG.info("adbOptions: " + adbOptions);

    TargetDeviceOptions targetDeviceOptions =
        new TargetDeviceOptions(
            cliOptions.useEmulatorsOnlyMode,
            cliOptions.useRealDevicesOnlyMode,
            Optional.ofNullable(cliOptions.serialNumber));
    LOG.info("targetDeviceOptions: " + targetDeviceOptions);

    this.stderr = new ByteArrayOutputStream();
    Console console =
        new Console(
            Verbosity.STANDARD_INFORMATION,
            new PrintStream(ByteStreams.nullOutputStream()),
            new PrintStream(stderr));
    SetDebugAppMode setDebugAppMode = SetDebugAppMode.SET;
    if (cliOptions.skipSetDebugApp) {
      setDebugAppMode = SetDebugAppMode.SKIP;
    }
    AdbUtils adbUtils =
        new AdbUtils(
            Optional.of(apkOptions.adbExecutable)
                .orElseThrow(AndroidInstallException.Companion::adbPathNotFound),
            adbOptions.getAdbServerPort());
    this.adbHelper =
        new AdbHelper(
            adbUtils,
            adbOptions,
            targetDeviceOptions,
            new AdbExecutionContext(console),
            new IsolatedAndroidInstallerPrinter(logger),
            apkOptions.restartAdbOnFailure,
            apkOptions.skipInstallMetadata,
            setDebugAppMode);
  }

  /** Uses AdbHelper to do actual install with APK */
  public synchronized InstallResult installApk() {
    List<Map<String, String>> deviceInfos = new ArrayList();
    try {
      if (cliOptions.cleanUp) {
        String appId =
            AdbHelper.tryToExtractPackageNameFromManifest(apkInfo.getManifestPath().getPath());
        adbHelper.uninstallApp(appId, cliOptions.keepUserData);
      } else {
        if (cliOptions.uninstallFirst) {
          String appId =
              AdbHelper.tryToExtractPackageNameFromManifest(apkInfo.getManifestPath().getPath());
          adbHelper.uninstallApp(appId, cliOptions.keepUserData);
        }
        logger.info(String.format("Attempting install of %s", apkInfo.getApkPath()));
        Instant start = Instant.now();

        Set<AndroidDeviceInfo> androidDeviceInfos = adbHelper.getAndroidDeviceInfo(apkInfo);
        for (AndroidDeviceInfo deviceInfo : androidDeviceInfos) {
          Map<String, String> infoMap =
              Map.ofEntries(
                  entry("installer", deviceInfo.getAndroidDeviceImplementation()),
                  entry("sdk", deviceInfo.getSdk()),
                  entry("abi", deviceInfo.getAbi()),
                  entry("locale", deviceInfo.getLocale()),
                  entry("build_fingerprint", deviceInfo.getBuildFingerprint()),
                  entry("is_emulator", deviceInfo.isEmulator() ? "1" : "0"),
                  entry("density", deviceInfo.getDensity().toString()));
          deviceInfos.add(infoMap);
        }
        for (AndroidDeviceInfo deviceInfo : androidDeviceInfos) {
          adbHelper.throwIfIncompatibleAbi(deviceInfo, apkInfo);
        }

        adbHelper.installApk(
            apkInfo,
            exopackageInfo,
            rootPath,
            installViaSd,
            /* quiet= */ false,
            installId.getValue());
        logger.info(
            String.format(
                "Install of %s finished in %d seconds",
                apkInfo.getApkPath().getFileName(),
                Duration.between(start, Instant.now()).getSeconds()));

        String packageName =
            AdbHelper.tryToExtractPackageNameFromManifest(apkInfo.getManifestPath().getPath());

        // Determine if app links should be enabled based on command line option or allowlist
        boolean shouldEnableAppLinks = false;
        if (cliOptions.enableAppLinks != null) {
          // Explicit option provided by user
          shouldEnableAppLinks = cliOptions.enableAppLinks;
        } else {
          // No option provided, check allowlist
          shouldEnableAppLinks = ENABLE_APP_LINKS_ALLOWLIST.contains(packageName);
        }

        if (shouldEnableAppLinks) {
          try {
            adbHelper.adbCall(
                "enable app links",
                (device) -> {
                  device.enableAppLinks(packageName);
                  return true;
                },
                true);
          } catch (Exception e) {
            logger.warning("Failed to enable app links: " + e.getMessage());
          }
        }

        if (cliOptions.run || cliOptions.activity != null || cliOptions.intentUri != null) {
          adbHelper.startActivityForIsolatedApk(
              apkInfo,
              installId.getValue(),
              cliOptions.activity,
              cliOptions.intentUri,
              cliOptions.waitForDebugger,
              cliOptions.skipSetDebugApp);
        }
      }
      return new InstallResult(deviceInfos, Optional.empty());
    } catch (AndroidInstallException exc) {
      return new InstallResult(deviceInfos, Optional.of(exc.getInstallError()));
    } catch (Exception err) {
      String errMsg =
          Optional.ofNullable(stderr.toString())
              .filter(s -> !s.isEmpty())
              .map(s -> "stderr message: " + s)
              .orElseGet(err::getMessage);
      logger.log(
          Level.SEVERE,
          String.format("Error while installing %s. Error message: %s", installId, errMsg),
          err);
      return new InstallResult(
          deviceInfos,
          Optional.of(errMsg).map(AndroidInstallErrorClassifier.INSTANCE::fromErrorMessage));
    }
  }
}
