/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.cd.workertool;

import com.facebook.buck.core.build.execution.context.IsolatedExecutionContext;
import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.step.StepExecutionResult;
import com.facebook.buck.step.isolatedsteps.IsolatedStep;
import com.facebook.buck.step.isolatedsteps.IsolatedStepsRunner;
import com.facebook.buck.util.ClassLoaderCache;
import com.facebook.buck.util.Console;
import com.facebook.buck.util.ProcessExecutor;
import com.google.common.collect.ImmutableList;
import javax.annotation.Nonnull;

/** Common methods used by CD worker command executors */
public class StepExecutionUtils {

  private StepExecutionUtils() {}

  public static StepExecutionResult executeSteps(
      ImmutableList<IsolatedStep> steps, IsolatedExecutionContext executionContext) {
    return IsolatedStepsRunner.executeWithDefaultExceptionHandling(steps, executionContext);
  }

  @Nonnull
  public static IsolatedExecutionContext createExecutionContext(
      ClassLoaderCache classLoaderCache,
      ProcessExecutor processExecutor,
      Console console,
      AbsPath ruleCellRoot) {
    return IsolatedExecutionContext.of(classLoaderCache, console, processExecutor, ruleCellRoot);
  }
}
