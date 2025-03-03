/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.step.isolatedsteps.common;

import static com.facebook.buck.io.filesystem.impl.ProjectFilesystemUtils.getAbsPathForRelativePath;

import com.facebook.buck.core.build.execution.context.IsolatedExecutionContext;
import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.core.util.immutables.BuckStyleValue;
import com.facebook.buck.io.filesystem.impl.ProjectFilesystemUtils;
import com.facebook.buck.step.StepExecutionResult;
import com.facebook.buck.step.StepExecutionResults;
import com.facebook.buck.step.isolatedsteps.IsolatedStep;
import java.io.IOException;

/** Creates a symlink from a desired path to an existing path. */
@BuckStyleValue
public abstract class SymlinkIsolatedStep implements IsolatedStep {

  abstract RelPath getExistingPath();

  abstract RelPath getDesiredPath();

  @Override
  public String getShortName() {
    return "symlink_file";
  }

  @Override
  public StepExecutionResult executeIsolatedStep(IsolatedExecutionContext context)
      throws IOException {
    AbsPath ruleCellRoot = context.getRuleCellRoot();

    AbsPath existingAbsPath = getAbsPathForRelativePath(ruleCellRoot, getExistingPath());
    AbsPath desiredAbsPath = getAbsPathForRelativePath(ruleCellRoot, getDesiredPath());

    ProjectFilesystemUtils.createSymLink(
        ruleCellRoot, desiredAbsPath.getPath(), existingAbsPath.getPath(), /* force */ true);

    return StepExecutionResults.SUCCESS;
  }

  @Override
  public String getIsolatedStepDescription(IsolatedExecutionContext context) {
    AbsPath ruleCellRoot = context.getRuleCellRoot();
    return String.join(
        " ",
        "ln",
        "-f",
        "-s",
        getAbsPathForRelativePath(ruleCellRoot, getExistingPath()).toString(),
        getAbsPathForRelativePath(ruleCellRoot, getDesiredPath()).toString());
  }

  public static SymlinkIsolatedStep of(RelPath existingPath, RelPath desiredPath) {
    return ImmutableSymlinkIsolatedStep.ofImpl(existingPath, desiredPath);
  }
}
