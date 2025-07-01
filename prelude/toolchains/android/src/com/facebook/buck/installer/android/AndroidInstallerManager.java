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

import com.facebook.buck.android.IsolatedApkInfo;
import com.facebook.buck.android.exopackage.IsolatedExopackageInfo;
import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.installer.InstallCommand;
import com.facebook.buck.installer.InstallError;
import com.facebook.buck.installer.InstallId;
import com.facebook.buck.installer.InstallResult;
import com.google.common.base.Preconditions;
import com.google.common.base.Throwables;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Sets;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger; // NOPMD
import java.util.stream.Collectors;

/**
 * Coordinates an Android Install of an APK. We need three artifacts: the apk,
 * install_android_options.json (configuration options), and an AndroidManifest.xml
 */
class AndroidInstallerManager implements InstallCommand {

  private static final Logger LOG = Logger.getLogger(AndroidInstallerManager.class.getName());
  private final AndroidCommandLineOptions options;
  private final AndroidInstallErrorClassifier errorClassifier =
      AndroidInstallErrorClassifier.INSTANCE;
  private final Map<InstallId, AndroidArtifacts> installIdToFutureMap = new HashMap<>();

  private final Map<String, String> SHORT_TO_FULL_ABI_MAP =
      new HashMap<>(
          Map.of("armv7", "armeabi-v7a", "arm64", "arm64-v8a", "x86", "x86", "x86_64", "x86_64"));

  private final Set<String> ABI_64s = new HashSet<>(Arrays.asList("arm64-v8a", "x86_64"));

  AndroidInstallerManager(AndroidCommandLineOptions options) {
    this.options = options;
  }

  /**
   * Coordinates the install artifacts needed for an install. The install_android_options.json is
   * parsed into a AndroidInstallApkOptions and the manifest is later set in the apkInstallOptions
   * as a separate field.
   */
  @Override
  public InstallResult fileReady(String artifactName, Path artifactPath, InstallId installId) {
    try {
      if (artifactName.equals("cpu_filters")) {
        return checkAbiCompatibility(AbsPath.of(artifactPath));
      }

      AndroidArtifacts androidArtifacts = getOrMakeAndroidArtifacts(installId);
      if (artifactName.equals("options")) {
        androidArtifacts.setApkOptions(
            new AndroidInstallApkOptions(artifactPath, options.adbExecutablePath));
        LOG.log(Level.INFO, androidArtifacts.getApkOptions().toString());
      } else if (artifactName.equals("manifest")) {
        androidArtifacts.setAndroidManifestPath(AbsPath.of(artifactPath));
      } else if (artifactName.equals("secondary_dex_exopackage_info_directory")) {
        androidArtifacts.setSecondaryDexExopackageInfoDirectory(
            Optional.of(AbsPath.of(artifactPath)));
      } else if (artifactName.equals("secondary_dex_exopackage_info_metadata")) {
        androidArtifacts.setSecondaryDexExopackageInfoMetadata(
            Optional.of(AbsPath.of(artifactPath)));
      } else if (artifactName.equals("native_library_exopackage_info_directory")) {
        androidArtifacts.setNativeLibraryExopackageInfoDirectory(
            Optional.of(AbsPath.of(artifactPath)));
      } else if (artifactName.equals("native_library_exopackage_info_metadata")) {
        androidArtifacts.setNativeLibraryExopackageInfoMetadata(
            Optional.of(AbsPath.of(artifactPath)));
      } else if (artifactName.equals("resources_exopackage_assets")) {
        androidArtifacts.setResourcesExopackageInfoAssets(Optional.of(AbsPath.of(artifactPath)));
      } else if (artifactName.equals("resources_exopackage_assets_hash")) {
        androidArtifacts.setResourcesExopackageInfoAssetsHash(
            Optional.of(AbsPath.of(artifactPath)));
      } else if (artifactName.equals("resources_exopackage_res")) {
        androidArtifacts.setResourcesExopackageInfoRes(Optional.of(AbsPath.of(artifactPath)));
      } else if (artifactName.equals("resources_exopackage_res_hash")) {
        androidArtifacts.setResourcesExopackageInfoResHash(Optional.of(AbsPath.of(artifactPath)));
      } else {
        androidArtifacts.setApk(AbsPath.of(artifactPath));
      }

      return InstallResult.success();
    } catch (Exception err) {
      String errMsg = Throwables.getStackTraceAsString(err);
      LOG.log(
          Level.SEVERE,
          String.format(
              "Error installing %s from %s due to %s", artifactName, artifactPath, errMsg),
          err);
      return InstallResult.error(errorClassifier.fromErrorMessage(errMsg));
    }
  }

  @Override
  public InstallResult allFilesReady(InstallId installId) {
    try {
      AndroidArtifacts androidArtifacts = getOrMakeAndroidArtifacts(installId);

      String adbPath = androidArtifacts.getApkOptions().adbExecutable;
      if (!Files.exists(Paths.get(adbPath))) {
        return InstallResult.error(
            errorClassifier.fromErrorMessage(
                String.format(
                    "Could not find `adb` in PATH, and could not find `adb` "
                        + "at %s. Please add `adb` to your PATH",
                    adbPath)));
      }

      Optional<AbsPath> secondaryDexExopackageInfoDirectory =
          androidArtifacts.getSecondaryDexExopackageInfoDirectory();
      Optional<AbsPath> secondaryDexExopackageInfoMetadata =
          androidArtifacts.getSecondaryDexExopackageInfoMetadata();
      Optional<AbsPath> nativeLibraryExopackageInfoDirectory =
          androidArtifacts.getNativeLibraryExopackageInfoDirectory();
      Optional<AbsPath> nativeLibraryExopackageInfoMetadata =
          androidArtifacts.getNativeLibraryExopackageInfoMetadata();
      ImmutableList.Builder<IsolatedExopackageInfo.IsolatedExopackagePathAndHash>
          pathAndHashBuilder = ImmutableList.builder();
      androidArtifacts
          .getResourcesExopackageInfoAssets()
          .ifPresent(
              assets ->
                  pathAndHashBuilder.add(
                      new IsolatedExopackageInfo.IsolatedExopackagePathAndHash(
                          assets, androidArtifacts.getResourcesExopackageInfoAssetsHash().get())));
      androidArtifacts
          .getResourcesExopackageInfoRes()
          .ifPresent(
              res ->
                  pathAndHashBuilder.add(
                      new IsolatedExopackageInfo.IsolatedExopackagePathAndHash(
                          res, androidArtifacts.getResourcesExopackageInfoResHash().get())));
      ImmutableList<IsolatedExopackageInfo.IsolatedExopackagePathAndHash> exopackageResources =
          pathAndHashBuilder.build();

      Optional<IsolatedExopackageInfo> isolatedExopackageInfo = Optional.empty();
      if (secondaryDexExopackageInfoDirectory.isPresent()
          || secondaryDexExopackageInfoMetadata.isPresent()
          || nativeLibraryExopackageInfoDirectory.isPresent()
          || nativeLibraryExopackageInfoMetadata.isPresent()
          || !exopackageResources.isEmpty()) {
        Preconditions.checkState(
            secondaryDexExopackageInfoDirectory.isPresent()
                == secondaryDexExopackageInfoMetadata.isPresent());
        Optional<IsolatedExopackageInfo.IsolatedDexInfo> dexInfo =
            secondaryDexExopackageInfoDirectory.map(
                directory ->
                    new IsolatedExopackageInfo.IsolatedDexInfo(
                        secondaryDexExopackageInfoMetadata.get(), directory));

        Preconditions.checkState(
            nativeLibraryExopackageInfoDirectory.isPresent()
                == nativeLibraryExopackageInfoMetadata.isPresent());
        Optional<IsolatedExopackageInfo.IsolatedNativeLibsInfo> nativeLibsInfo =
            nativeLibraryExopackageInfoDirectory.map(
                absPath ->
                    new IsolatedExopackageInfo.IsolatedNativeLibsInfo(
                        nativeLibraryExopackageInfoMetadata.get(), absPath));

        Optional<IsolatedExopackageInfo.IsolatedResourcesInfo> resourcesInfo;
        if (!exopackageResources.isEmpty()) {
          resourcesInfo =
              Optional.of(new IsolatedExopackageInfo.IsolatedResourcesInfo(exopackageResources));
        } else {
          resourcesInfo = Optional.empty();
        }

        isolatedExopackageInfo =
            Optional.of(new IsolatedExopackageInfo(dexInfo, nativeLibsInfo, resourcesInfo));
      }
      AndroidInstall androidInstaller =
          new AndroidInstall(
              LOG,
              AbsPath.of(Paths.get(".").normalize().toAbsolutePath()),
              options,
              androidArtifacts.getApkOptions(),
              IsolatedApkInfo.of(
                  androidArtifacts.getAndroidManifestPath(), androidArtifacts.getApk()),
              isolatedExopackageInfo,
              installId);
      return androidInstaller.installApk();

    } catch (Exception err) {
      String errMsg = Throwables.getStackTraceAsString(err);
      LOG.log(Level.SEVERE, String.format("Install error due to %s", errMsg), err);
      return InstallResult.error(errorClassifier.fromErrorMessage(errMsg));
    }
  }

  private AndroidArtifacts getOrMakeAndroidArtifacts(InstallId install_id) {
    synchronized (installIdToFutureMap) {
      return installIdToFutureMap.computeIfAbsent(install_id, ignore -> new AndroidArtifacts());
    }
  }

  private InstallResult checkAbiCompatibility(AbsPath apkAbiPath) {
    try {
      Set<String> apkAbis =
          Files.readAllLines(apkAbiPath.getPath()).stream()
              .map(abi -> SHORT_TO_FULL_ABI_MAP.get(abi.trim()))
              .collect(Collectors.toSet());
      Set<String> deviceAbis = getDeviceAbis();
      if (deviceAbis.isEmpty()) {
        // we couldn't determine the device ABI, so returning success.
        return InstallResult.success();
      }
      Set<String> commonAbis =
          apkAbis.stream().filter(deviceAbis::contains).collect(Collectors.toSet());
      if (commonAbis.isEmpty()) {
        String message =
            String.format(
                "You are trying to install an APK with incompatible native libraries - "
                    + "the APK has native libraries built for %s, but the device has CPU %s.",
                String.join(",", apkAbis), String.join(",", deviceAbis));
        return InstallResult.error(
            new InstallError(message, AndroidInstallErrorTag.INCOMPATIBLE_NATIVE_LIB));
      }
    } catch (IOException e) {
      // unable to determine if we should fail early. no-op
    }
    return InstallResult.success();
  }

  private Set<String> getDeviceAbis() {
    // Unless serial is specified,
    // use first device from -d or -e flag
    Set<String> abis = new HashSet<>();
    String serialNumber = options.serialNumber;
    if (serialNumber == null) {
      try {
        String command = "adb devices";
        Process process = Runtime.getRuntime().exec(command);
        BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()));
        String line;
        while ((line = reader.readLine()) != null) {
          String[] output = line.split("\t");
          String currentSerialNumber = output[0].trim();
          if (output.length == 2 && output[1].trim().equals("device")) {
            if (!options.useEmulatorsOnlyMode && !options.useRealDevicesOnlyMode) {
              serialNumber = currentSerialNumber;
              break;
            }
            if (options.useEmulatorsOnlyMode && currentSerialNumber.startsWith("emulator")) {
              serialNumber = currentSerialNumber;
              break;
            }
            if (options.useRealDevicesOnlyMode && !currentSerialNumber.startsWith("emulator")) {
              serialNumber = currentSerialNumber;
              break;
            }
          }
        }
      } catch (Exception _e) {
        // no op
      }
    }

    try {
      // get default abi
      String abiCommand = String.format("adb -s %s shell getprop ro.product.cpu.abi", serialNumber);
      Process abiProcess = Runtime.getRuntime().exec(abiCommand);
      BufferedReader abiReader =
          new BufferedReader(new InputStreamReader(abiProcess.getInputStream()));
      String abi = abiReader.readLine();
      if (abi != null) {
        abis.add(abi.trim());
      }
    } catch (Exception _e) {
      // no op
    }

    if (!Sets.intersection(abis, ABI_64s).isEmpty()) {
      // Need to call both ro.product.cpu.abi and ro.product.cpu.abilist
      // as sticking to ro.product.cpu.abi helped fixing the issue of
      // exopackage install when the app was already installed in the device.
      try {
        // add 32bit abi if device supports it
        String abiCommand =
            String.format("adb -s %s shell getprop ro.product.cpu.abilist", serialNumber);
        Process abiProcess = Runtime.getRuntime().exec(abiCommand);
        BufferedReader abiReader =
            new BufferedReader(new InputStreamReader(abiProcess.getInputStream()));
        Set<String> abiList = new HashSet<>(Arrays.asList(abiReader.readLine().split(",")));
        if (abis.contains("x86_64") && abiList.contains("x86")) {
          abis.add("x86");
        } else if (abis.contains("arm64-v8a") && abiList.contains("armeabi-v7a")) {
          abis.add("armeabi-v7a");
        }
      } catch (Exception _e) {
        // no op
      }
    }

    return abis;
  }
}
