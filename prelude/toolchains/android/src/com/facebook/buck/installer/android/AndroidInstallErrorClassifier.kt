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
    for ((pattern, handler) in errorPatterns) {
      if (input.contains(pattern)) {
        return handler(input)
      }
    }
    return InstallError(input, AndroidInstallErrorTag.OTHER_INFRA)
  }

  private val errorPatterns =
      listOf<Pair<String, (String) -> InstallError>>(
          "stderr message: " to this::decorateStdErrMessages,
          "com.android.ddmlib.InstallException" to this::decorateAdbInstallException,
          "com.facebook.buck.core.exceptions.HumanReadableException" to
              this::decorateHumanReadableException)

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

  private fun decorateAdbInstallException(input: String): InstallError {
    val regex = Regex("com\\.android\\.ddmlib\\.InstallException:( [A-Z_}]+:)?([^\n]*)")
    val matchResult = regex.find(input)
    return matchResult?.let {
      val adbTag = it.groupValues[1].trim()
      val message = it.groupValues[2].trim().ifEmpty { "Unknown ADB Install Exception." }
      when {
        message.contains("device offline") ->
            createInstallError(AndroidInstallErrorTag.DEVICE_OFFLINE, message)
        message.contains("Connection reset by peer") ->
            createInstallError(AndroidInstallErrorTag.ADB_CONNECTION_RESET_BY_PEER, message)
        Regex("Device .* not found in .* attached servers").containsMatchIn(input) ->
            createInstallError(AndroidInstallErrorTag.DEVICE_NOT_FOUND, message)
        adbTag == "INSTALL_FAILED_OLDER_SDK" ->
            createInstallError(AndroidInstallErrorTag.REQUIRE_NEWER_SDK, message)
        adbTag == "INSTALL_FAILED_UPDATE_INCOMPATIBLE" ->
            createInstallError(AndroidInstallErrorTag.UPDATE_INCOMPATIBLE, message)
        adbTag == "INSTALL_FAILED_MISSING_SHARED_LIBRARY" ->
            createInstallError(AndroidInstallErrorTag.MISSING_SHARED_LIBRARY, message)
        adbTag == "INSTALL_FAILED_VERIFICATION_FAILURE" ->
            createInstallError(AndroidInstallErrorTag.VERIFICATION_FAILED, message)
        adbTag == "INSTALL_FAILED_INVALID_APK" ->
            createInstallError(AndroidInstallErrorTag.INVALID_APK, message)
        adbTag == "INSTALL_FAILED_USER_RESTRICTED" ->
            createInstallError(AndroidInstallErrorTag.INSTALL_CANCELLED_BY_USER, message)
        message.contains("com.android.ddmlib.TimeoutException") ->
            createInstallError(
                AndroidInstallErrorTag.ADB_CONNECTION_TIMED_OUT, "Connection with device timed out")
        else ->
            InstallError("Unknown Install Exception: ${input}", AndroidInstallErrorTag.OTHER_INFRA)
      }
    } ?: InstallError("Unknown Install Exception: ${input}", AndroidInstallErrorTag.OTHER_INFRA)
  }

  private fun decorateHumanReadableException(input: String): InstallError {
    return when {
      input.contains("Write failed: No space left on device") ->
          createInstallError(
              AndroidInstallErrorTag.NO_SPACE_LEFT_ON_DEVICE,
              "Write failed: No space left on device")
      else -> createInstallError(AndroidInstallErrorTag.OTHER_INFRA, input)
    }
  }

  private fun createInstallError(tag: AndroidInstallErrorTag, msg: String): InstallError {
    return InstallError(msg, tag)
  }
}
