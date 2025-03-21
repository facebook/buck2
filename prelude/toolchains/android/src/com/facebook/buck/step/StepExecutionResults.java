/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.step;

/** A collection of common StepExecutionResult constants. */
public class StepExecutionResults {
  // NB: These constants cannot live in StepExecutionResult, as referencing subclass in
  // static initializer may cause deadlock during classloading.

  public static final int SUCCESS_EXIT_CODE = 0;
  public static final int ERROR_EXIT_CODE = 1;

  public static final StepExecutionResult SUCCESS = StepExecutionResult.of(SUCCESS_EXIT_CODE);
  public static final StepExecutionResult ERROR = StepExecutionResult.of(ERROR_EXIT_CODE);

  private StepExecutionResults() {} // Utility class. Do not instantiate.
}
