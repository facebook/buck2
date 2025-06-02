/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android.exopackage;

import com.android.ddmlib.IDevice;
import com.facebook.buck.android.AndroidInstallPrinter;
import com.facebook.buck.util.Console;
import java.nio.file.Path;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;

public interface AndroidDeviceFactory {
  AndroidDevice createAndroidDevice(
      AndroidInstallPrinter androidInstallPrinter,
      @Nonnull IDevice device,
      Console console,
      @Nullable Path agentApkPath,
      int agentPort,
      boolean isZstdCompressionEnabled,
      int maxRetries,
      long retryDelayMs,
      String adbExecutable,
      int adbServerPort,
      AdbUtils adbUtils);
}
