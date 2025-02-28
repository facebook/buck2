/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android.exopackage;

import com.google.common.annotations.VisibleForTesting;

@VisibleForTesting
public class PackageInfo {
  public final String apkPath;
  public final String nativeLibPath;
  public final String versionCode;

  public PackageInfo(String apkPath, String nativeLibPath, String versionCode) {
    this.nativeLibPath = nativeLibPath;
    this.apkPath = apkPath;
    this.versionCode = versionCode;
  }
}
