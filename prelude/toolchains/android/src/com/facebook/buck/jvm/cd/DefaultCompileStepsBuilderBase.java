/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.cd;

import com.facebook.buck.jvm.java.CompileToJarStepFactory;
import com.facebook.buck.step.isolatedsteps.IsolatedStep;
import com.google.common.collect.ImmutableList;

/** Creates a list of {@link IsolatedStep} that is ready for in process execute. */
abstract class DefaultCompileStepsBuilderBase<T extends CompileToJarStepFactory.ExtraParams>
    implements CompileStepsBuilder {

  protected final ImmutableList.Builder<IsolatedStep> stepsBuilder = ImmutableList.builder();
  protected final CompileToJarStepFactory<T> configuredCompiler;

  DefaultCompileStepsBuilderBase(CompileToJarStepFactory<T> configuredCompiler) {
    this.configuredCompiler = configuredCompiler;
  }

  @Override
  public final ImmutableList<IsolatedStep> buildIsolatedSteps() {
    return stepsBuilder.build();
  }
}
