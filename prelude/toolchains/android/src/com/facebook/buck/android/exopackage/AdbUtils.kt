/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android.exopackage

import java.io.BufferedReader
import java.io.InputStreamReader

data class AdbCommandResult(val exitCode: Int, val output: String, val error: String? = null)

class AdbCommandFailedException(message: String) : Exception(message)

object AdbUtils {
  fun executeAdbShellCommand(command: String, deviceId: String): String {
    return executeAdbCommand("shell $command", deviceId)
  }

  fun executeAdbCommand(command: String, deviceId: String): String {
    val adbCommandResult: AdbCommandResult =
        try {
          runAdbCommand("-s $deviceId $command")
        } catch (e: Exception) {
          error("Failed to execute adb command 'adb $command' on device $deviceId.\n${e.message}")
        }
    if (adbCommandResult.exitCode != 0) {
      throw AdbCommandFailedException(
          "Executing 'adb $command' on $deviceId failed with code ${adbCommandResult.exitCode}." +
              if (!adbCommandResult.error.isNullOrEmpty()) "\nError:${adbCommandResult.error}"
              else "")
    } else {
      return adbCommandResult.output
    }
  }

  private fun runAdbCommand(input: String): AdbCommandResult {
    val command = "adb $input"
    val processBuilder = ProcessBuilder(*command.split(" ").toTypedArray())
    val process = processBuilder.start()
    val output = StringBuilder()
    BufferedReader(InputStreamReader(process.inputStream)).use { reader ->
      var line: String? = reader.readLine()
      while (line != null) {
        output.append(line).append("\n")
        line = reader.readLine()
      }
    }
    val errorOutput = StringBuilder()
    BufferedReader(InputStreamReader(process.errorStream)).use { reader ->
      var line: String? = reader.readLine()
      while (line != null) {
        errorOutput.append(line).append("\n")
        line = reader.readLine()
      }
    }
    val exitCode = process.waitFor()
    return AdbCommandResult(
        exitCode,
        output = output.toString().trim(),
        error = if (errorOutput.isNotEmpty()) errorOutput.toString() else null)
  }
}
