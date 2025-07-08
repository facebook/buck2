/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.step

import java.util.Optional

/** Exit code, command and stderr info from the executed step */
data class StepExecutionResult(
    val exitCode: Int,
    val stderr: Optional<String>,
    val cause: Optional<Exception>
) {
  constructor(
      exitCode: Int,
      stderr: Optional<String>
  ) : this(exitCode, stderr, Optional.empty<Exception>())

  val isSuccess: Boolean
    get() = exitCode == StepExecutionResults.SUCCESS_EXIT_CODE

  val errorMessage: String
    get() {
      return cause
          .map<String>(Throwable::message)
          .or(this::stderr)
          .orElse(String.format("<failed with exit code %s>", exitCode))
    }

  companion object {
    /** Creates `StepExecutionResult` from `exitCode` */
    @JvmStatic
    fun of(exitCode: Int): StepExecutionResult {
      return StepExecutionResult(
          exitCode,
          if (exitCode == 0) Optional.empty() else Optional.of("Failed to execute isolated step."),
          Optional.empty())
    }

    /** Creates `StepExecutionResult` from `exception` */
    @JvmStatic
    fun of(exception: Throwable): StepExecutionResult {
      return StepExecutionResult(
          StepExecutionResults.ERROR_EXIT_CODE,
          Optional.of("Failed to execute isolated step."),
          Optional.ofNullable(exception.cause as Exception?))
    }
  }
}
