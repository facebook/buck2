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

import com.facebook.buck.installer.InstallError

object AndroidInstallErrorClassifier {
  fun fromErrorMessage(input: String): InstallError {
    if (input.contains("No space left on device")) {
      return createInstallError(
          AndroidInstallErrorTag.NO_SPACE_LEFT_ON_DEVICE,
          "No space left on device. Free up space on the device and try again.",
      )
    }
    for ((pattern, handler) in errorPatterns) {
      if (input.contains(pattern)) {
        return handler(input)
      }
    }
    return InstallError(input, AndroidInstallErrorTag.OTHER_INFRA)
  }

  private val errorPatterns = listOf<Pair<String, (String) -> InstallError>>(
      "stderr message: " to this::decorateStdErrMessages,
  )

  private fun decorateStdErrMessages(input: String): InstallError {
    val message = input.substringAfter("stderr message: ")
    return when {
      message.contains("Could not find `adb` in PATH") ->
          createInstallError(AndroidInstallErrorTag.ADB_NOT_FOUND, message)
      message.contains("Failed to connect to adb.") ->
          createInstallError(AndroidInstallErrorTag.FAILED_TO_CONNECT_TO_ADB, message)
      message.contains("Didn't find any attached Android devices/emulators.") ->
          createInstallError(AndroidInstallErrorTag.DEVICE_NOT_FOUND, message)
      message.contains("devices match specified device filter") ->
          createInstallError(AndroidInstallErrorTag.MULTIPLE_DEVICES_MATCH_FILTER, message)
      Regex("You are trying to install .* onto a device with the following CPU")
          .containsMatchIn(message) ->
          createInstallError(AndroidInstallErrorTag.INCOMPATIBLE_ABI, message)
      Regex("You are trying to install an APK with incompatible native libraries")
          .containsMatchIn(message) ->
          createInstallError(AndroidInstallErrorTag.INCOMPATIBLE_NATIVE_LIB, message)
      else -> createInstallError(AndroidInstallErrorTag.OTHER_INFRA, message)
    }
  }

  private fun createInstallError(tag: AndroidInstallErrorTag, msg: String): InstallError {
    return InstallError(msg, tag)
  }
}
