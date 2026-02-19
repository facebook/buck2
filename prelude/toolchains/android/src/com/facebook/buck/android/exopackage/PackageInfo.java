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
import com.google.common.annotations.VisibleForTesting;

@VisibleForTesting
@Nullsafe(Nullsafe.Mode.LOCAL)
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
