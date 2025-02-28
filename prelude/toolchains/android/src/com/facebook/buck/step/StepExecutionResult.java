/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.step;

import com.facebook.buck.core.util.immutables.BuckStyleValueWithBuilder;
import com.facebook.buck.util.ProcessExecutor;
import com.google.common.collect.ImmutableList;
import java.util.Optional;

/** Exit code, command and stderr info from the executed step */
@BuckStyleValueWithBuilder
public interface StepExecutionResult {

  int getExitCode();

  ImmutableList<String> getExecutedCommand();

  Optional<String> getStderr();

  Optional<Exception> getCause();

  default boolean isSuccess() {
    return getExitCode() == StepExecutionResults.SUCCESS_EXIT_CODE;
  }

  /** Creates {@code StepExecutionResult} from {@code exitCode} */
  static StepExecutionResult of(int exitCode) {
    return new Builder().setExitCode(exitCode).build();
  }

  /** Creates {@code StepExecutionResult} from {@code exception} */
  static StepExecutionResult of(Throwable exception) {
    return StepExecutionResult.builder()
        .setExitCode(StepExecutionResults.ERROR_EXIT_CODE)
        .setCause((Exception) exception.getCause())
        .build();
  }

  /** Creates {@code StepExecutionResult} from {@code ProcessExecutor.Result} */
  static StepExecutionResult of(ProcessExecutor.Result result) {
    return new Builder()
        .setExitCode(result.getExitCode())
        .setExecutedCommand(result.getCommand())
        .setStderr(result.getStderr())
        .build();
  }

  static Builder builder() {
    return new Builder();
  }

  class Builder extends ImmutableStepExecutionResult.Builder {}

  public default String getErrorMessage() {
    return getCause()
        .map(Throwable::getMessage)
        .or(this::getStderr)
        .orElse(String.format("<failed with exit code %s>", getExitCode()));
  }
}
