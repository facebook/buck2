/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.step.isolatedsteps;

import com.facebook.buck.core.exceptions.ExceptionWithContext;
import com.facebook.buck.core.exceptions.HumanReadableException;
import com.facebook.buck.core.exceptions.WrapsException;
import com.facebook.buck.step.StepExecutionResult;
import com.facebook.buck.util.string.MoreStrings;
import com.google.common.annotations.VisibleForTesting;
import java.util.Optional;
import java.util.OptionalInt;

public class StepFailedException extends Exception implements WrapsException, ExceptionWithContext {

  @VisibleForTesting static final int KEEP_FIRST_CHARS = 4 * 80;

  private final IsolatedStep step;
  private final String description;
  private final OptionalInt exitCode;

  /** Callers should use {@link #createForFailingIsolatedStepWithExitCode} unless in a unit test. */
  private StepFailedException(
      Throwable cause, IsolatedStep step, String description, OptionalInt exitCode) {
    super(cause);
    this.step = step;
    this.description = description;
    this.exitCode = exitCode;
  }

  @Override
  public String getMessage() {
    return getCause().getMessage() + System.lineSeparator() + "  " + getContext().get();
  }

  /** Creates a StepFailedException based on a StepExecutionResult. */
  public static StepFailedException createForFailingIsolatedStepWithExitCode(
      IsolatedStep step, String descriptionForStep, StepExecutionResult executionResult) {
    StringBuilder errorMessage = new StringBuilder();
    errorMessage.append(String.format("Failed to execute isolated step <%s>", step.getShortName()));
    Optional<String> stderr = executionResult.getStderr();
    if (stderr.isPresent()) {
      String error = stderr.get();
      errorMessage.append(System.lineSeparator()).append(error);
    }
    return new StepFailedException(
        getHumanReadableException(executionResult, errorMessage.toString()),
        step,
        descriptionForStep,
        OptionalInt.of(executionResult.getExitCode()));
  }

  private static HumanReadableException getHumanReadableException(
      StepExecutionResult executionResult, String errorMessage) {
    Optional<Exception> executionResultCause = executionResult.getCause();
    if (executionResultCause.isPresent()) {
      Exception cause = executionResultCause.get();
      if (cause instanceof HumanReadableException) {
        return (HumanReadableException) cause;
      }
      return new HumanReadableException(cause, errorMessage);
    }
    return new HumanReadableException(errorMessage);
  }

  private static void appendToErrorMessage(
      StringBuilder sb, String name, String value, boolean truncate) {
    sb.append(System.lineSeparator())
        .append(System.lineSeparator())
        .append(name)
        .append(": ")
        .append(truncate ? MoreStrings.truncateTail(value, KEEP_FIRST_CHARS) : value);
  }

  public static StepFailedException createForFailingStepWithException(
      IsolatedStep step, String descriptionForStep, Throwable throwable) {
    return new StepFailedException(throwable, step, descriptionForStep, OptionalInt.empty());
  }

  public IsolatedStep getStep() {
    return step;
  }

  public OptionalInt getExitCode() {
    return exitCode;
  }

  @Override
  public Optional<String> getContext() {
    return Optional.of(String.format("When running <%s>.", description));
  }
}
