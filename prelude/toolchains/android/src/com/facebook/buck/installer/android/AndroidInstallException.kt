/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.installer.android

import com.facebook.buck.core.util.log.Logger
import com.facebook.buck.installer.InstallError

class AndroidInstallException(val installError: InstallError) :
    RuntimeException(installError.message) {
  init {
    LOG.error(installError.message)
  }

  companion object {
    private val LOG: Logger = Logger.get(AndroidInstallException::class.java.name)

    fun rebootRequired(msg: String) =
        AndroidInstallException(InstallError(msg, AndroidInstallErrorTag.MANUAL_REBOOT_REQUIRED))

    fun tempFolderNotWritable(): AndroidInstallException =
        AndroidInstallException(
            InstallError(
                "Temp folder is not writable.", AndroidInstallErrorTag.TEMP_FOLDER_NOT_WRITABLE))

    fun operationNotSupported(operation: String): AndroidInstallException =
        AndroidInstallException(
            InstallError(
                "Operation $operation is not supported.", AndroidInstallErrorTag.OTHER_INFRA))

    fun deviceAbiUnknown() =
        AndroidInstallException(
            InstallError("Device ABI is unknown.", AndroidInstallErrorTag.UNKNOWN_DEVICE_ABI))

    fun adbPathNotFound() =
        AndroidInstallException(
            InstallError("Adb path not found.", AndroidInstallErrorTag.ADB_NOT_FOUND))

    fun adbCommandFailedException(
        message: String,
        exceptionMessage: String?
    ): AndroidInstallException {
      val errorMessage = exceptionMessage?.let { "\n" + it } ?: ""
      return AndroidInstallException(
          InstallError("$message.$errorMessage", AndroidInstallErrorTag.ADB_COMMAND_FAILED))
    }
  }
}
