/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android.exopackage

import com.android.ddmlib.IDevice
import com.facebook.buck.android.AndroidInstallPrinter
import com.facebook.buck.core.util.log.Logger
import com.facebook.buck.util.Console
import java.nio.file.Path

class AndroidDeviceFactoryImpl : AndroidDeviceFactory {
  override fun createAndroidDevice(
      androidInstallPrinter: AndroidInstallPrinter?,
      device: IDevice,
      console: Console?,
      agentApkPath: Path?,
      agentPort: Int,
      isZstdCompressionEnabled: Boolean,
      maxRetries: Int,
      retryDelayMs: Long,
      adbExecutable: String?,
      adbServerPort: Int,
      adbUtils: AdbUtils,
  ): AndroidDevice? {
    LOG.info("Creating AndroidDeviceImpl for %s", device.serialNumber)
    return AndroidDeviceImpl(device.serialNumber, adbUtils)
  }

  companion object {
    private val LOG: Logger = Logger.get(AndroidDeviceFactoryImpl::class.java.name)
  }
}
