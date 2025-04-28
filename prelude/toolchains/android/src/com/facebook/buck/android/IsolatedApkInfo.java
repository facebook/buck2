/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android;

import com.facebook.buck.core.filesystems.AbsPath;
import java.util.Objects;

/** Isolated Apk Info. */
public class IsolatedApkInfo {

  private final AbsPath manifestPath;
  private final AbsPath apkPath;

  public IsolatedApkInfo(final AbsPath manifestPath, final AbsPath apkPath) {
    this.manifestPath = manifestPath;
    this.apkPath = apkPath;
  }

  /**
   * @return the path to the AndroidManifest.xml. Note that this file might be a symlink, and might
   *     not exist at all before this rule has been built.
   */
  public AbsPath getManifestPath() {
    return manifestPath;
  }

  /**
   * @return The APK at this path is the final one that points to an APK that a user should install.
   */
  public AbsPath getApkPath() {
    return apkPath;
  }

  @Override
  public int hashCode() {
    return Objects.hash(manifestPath, apkPath);
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null || getClass() != obj.getClass()) {
      return false;
    }
    final IsolatedApkInfo other = (IsolatedApkInfo) obj;
    return Objects.equals(manifestPath, other.manifestPath)
        && Objects.equals(apkPath, other.apkPath);
  }

  public static IsolatedApkInfo of(AbsPath manifestPath, AbsPath apkPath) {
    return new IsolatedApkInfo(manifestPath, apkPath);
  }
}
