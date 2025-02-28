/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.kotlin.ksp

import com.facebook.buck.core.build.execution.context.IsolatedExecutionContext
import com.facebook.buck.step.StepExecutionResult
import com.facebook.buck.step.StepExecutionResults
import com.facebook.buck.step.isolatedsteps.IsolatedStep

class Ksp2Step(vararg parameters: Any?) : IsolatedStep() {
  override fun getShortName(): String = "ksp2_noop"

  override fun executeIsolatedStep(context: IsolatedExecutionContext?): StepExecutionResult =
      StepExecutionResults.SUCCESS

  override fun getIsolatedStepDescription(context: IsolatedExecutionContext?): String =
      "Non operational KSP2 step. Not meant to be executed"
}
