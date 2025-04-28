/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.step.isolatedsteps;

import com.facebook.buck.core.build.execution.context.IsolatedExecutionContext;
import com.facebook.buck.step.StepExecutionResult;
import java.io.IOException;

/**
 * Isolated step - build rule's step that is isolated from the buck core data structures as an
 * action graph.
 */
public interface IsolatedStep {

  /**
   * @return a short name/description for the command, such as "javac". Should fit on one line.
   */
  String getShortName();

  StepExecutionResult executeIsolatedStep(IsolatedExecutionContext context)
      throws IOException, InterruptedException;

  String getIsolatedStepDescription(IsolatedExecutionContext context);
}
