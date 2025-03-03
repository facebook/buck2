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
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.core.util.immutables.BuckStyleValue;
import com.facebook.buck.io.filesystem.impl.ProjectFilesystemUtils;
import com.facebook.buck.step.StepExecutionResult;
import com.facebook.buck.step.StepExecutionResults;
import com.facebook.buck.step.isolatedsteps.IsolatedStep;
import com.facebook.buck.util.Escaper;
import java.io.IOException;
import java.nio.file.Files;

/** Command that runs equivalent command of {@code mkdir -p} on the specified directory. */
@BuckStyleValue
public abstract class MkdirIsolatedStep extends IsolatedStep {

  public abstract RelPath getDirPath();

  @Override
  public StepExecutionResult executeIsolatedStep(IsolatedExecutionContext context)
      throws IOException {
    Files.createDirectories(
        ProjectFilesystemUtils.getPathForRelativePath(
            context.getRuleCellRoot(), getDirPath().getPath()));
    return StepExecutionResults.SUCCESS;
  }

  @Override
  public String getIsolatedStepDescription(IsolatedExecutionContext context) {
    return String.format("mkdir -p %s", Escaper.escapeAsShellString(getDirPath().toString()));
  }

  @Override
  public String getShortName() {
    return "mkdir";
  }

  public static MkdirIsolatedStep of(RelPath directoryToCreate) {
    return ImmutableMkdirIsolatedStep.ofImpl(directoryToCreate);
  }
}
