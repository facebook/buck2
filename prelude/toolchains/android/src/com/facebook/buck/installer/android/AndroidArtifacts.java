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

import com.facebook.buck.core.filesystems.AbsPath;
import java.util.Optional;

/** Holds android install related artifacts (apk options, manifest path, etc) */
class AndroidArtifacts {
  private AbsPath androidManifestPath;
  private AndroidInstallApkOptions apkOptions;
  private AbsPath apk;
  private Optional<AbsPath> secondaryDexExopackageInfoDirectory = Optional.empty();
  private Optional<AbsPath> secondaryDexExopackageInfoMetadata = Optional.empty();
  private Optional<AbsPath> nativeLibraryExopackageInfoDirectory = Optional.empty();
  private Optional<AbsPath> nativeLibraryExopackageInfoMetadata = Optional.empty();
  private Optional<AbsPath> resourcesExopackageInfoAssets = Optional.empty();
  private Optional<AbsPath> resourcesExopackageInfoAssetsHash = Optional.empty();
  private Optional<AbsPath> resourcesExopackageInfoRes = Optional.empty();
  private Optional<AbsPath> resourcesExopackageInfoResHash = Optional.empty();

  public void setAndroidManifestPath(AbsPath androidManifestPath) {
    this.androidManifestPath = androidManifestPath;
  }

  public AbsPath getAndroidManifestPath() {
    return this.androidManifestPath;
  }

  public void setApkOptions(AndroidInstallApkOptions apkOptions) {
    this.apkOptions = apkOptions;
  }

  public AndroidInstallApkOptions getApkOptions() {
    return this.apkOptions;
  }

  public AbsPath getApk() {
    return apk;
  }

  public void setApk(AbsPath apk) {
    this.apk = apk;
  }

  public Optional<AbsPath> getSecondaryDexExopackageInfoDirectory() {
    return secondaryDexExopackageInfoDirectory;
  }

  public void setSecondaryDexExopackageInfoDirectory(
      Optional<AbsPath> secondaryDexExopackageInfoDirectory) {
    this.secondaryDexExopackageInfoDirectory = secondaryDexExopackageInfoDirectory;
  }

  public Optional<AbsPath> getSecondaryDexExopackageInfoMetadata() {
    return secondaryDexExopackageInfoMetadata;
  }

  public void setSecondaryDexExopackageInfoMetadata(
      Optional<AbsPath> secondaryDexExopackageInfoMetadata) {
    this.secondaryDexExopackageInfoMetadata = secondaryDexExopackageInfoMetadata;
  }

  public Optional<AbsPath> getNativeLibraryExopackageInfoDirectory() {
    return nativeLibraryExopackageInfoDirectory;
  }

  public void setNativeLibraryExopackageInfoDirectory(
      Optional<AbsPath> nativeLibraryExopackageInfoDirectory) {
    this.nativeLibraryExopackageInfoDirectory = nativeLibraryExopackageInfoDirectory;
  }

  public Optional<AbsPath> getNativeLibraryExopackageInfoMetadata() {
    return nativeLibraryExopackageInfoMetadata;
  }

  public void setNativeLibraryExopackageInfoMetadata(
      Optional<AbsPath> nativeLibraryExopackageInfoMetadata) {
    this.nativeLibraryExopackageInfoMetadata = nativeLibraryExopackageInfoMetadata;
  }

  public Optional<AbsPath> getResourcesExopackageInfoAssets() {
    return resourcesExopackageInfoAssets;
  }

  public void setResourcesExopackageInfoAssets(Optional<AbsPath> resourcesExopackageInfoAssets) {
    this.resourcesExopackageInfoAssets = resourcesExopackageInfoAssets;
  }

  public Optional<AbsPath> getResourcesExopackageInfoAssetsHash() {
    return resourcesExopackageInfoAssetsHash;
  }

  public void setResourcesExopackageInfoAssetsHash(
      Optional<AbsPath> resourcesExopackageInfoAssetsHash) {
    this.resourcesExopackageInfoAssetsHash = resourcesExopackageInfoAssetsHash;
  }

  public Optional<AbsPath> getResourcesExopackageInfoRes() {
    return resourcesExopackageInfoRes;
  }

  public void setResourcesExopackageInfoRes(Optional<AbsPath> resourcesExopackageInfoRes) {
    this.resourcesExopackageInfoRes = resourcesExopackageInfoRes;
  }

  public Optional<AbsPath> getResourcesExopackageInfoResHash() {
    return resourcesExopackageInfoResHash;
  }

  public void setResourcesExopackageInfoResHash(Optional<AbsPath> resourcesExopackageInfoResHash) {
    this.resourcesExopackageInfoResHash = resourcesExopackageInfoResHash;
  }
}
