/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.step.isolatedsteps.common;

import com.facebook.buck.core.build.execution.context.IsolatedExecutionContext;
import com.facebook.buck.step.StepExecutionResult;
import com.facebook.buck.step.isolatedsteps.IsolatedStep;
import java.io.IOException;

/**
 * Abstract implementation of {@link IsolatedStep} that takes the description as a constructor
 * parameter and requires only the implementation of {@link
 * #executeIsolatedStep(IsolatedExecutionContext)}. This facilitates the creation of an anonymous
 * implementation of {@link IsolatedStep}.
 */
public abstract class AbstractIsolatedExecutionStep implements IsolatedStep {

  private final String description;

  public AbstractIsolatedExecutionStep(String description) {
    this.description = description;
  }

  @Override
  public abstract StepExecutionResult executeIsolatedStep(IsolatedExecutionContext context)
      throws IOException, InterruptedException;

  @Override
  public String getShortName() {
    return description;
  }

  @Override
  public String getIsolatedStepDescription(IsolatedExecutionContext context) {
    return description;
  }
}
