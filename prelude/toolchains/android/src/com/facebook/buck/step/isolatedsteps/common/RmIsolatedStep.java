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
import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.core.util.immutables.BuckStyleValue;
import com.facebook.buck.io.file.MostFiles;
import com.facebook.buck.io.filesystem.impl.ProjectFilesystemUtils;
import com.facebook.buck.step.StepExecutionResult;
import com.facebook.buck.step.StepExecutionResults;
import com.facebook.buck.step.isolatedsteps.IsolatedStep;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableSet;
import java.io.IOException;
import java.nio.file.Files;
import java.util.StringJoiner;

/** Removes a path if it exists. */
@BuckStyleValue
public abstract class RmIsolatedStep implements IsolatedStep {

  abstract RelPath getPath();

  abstract boolean isRecursive();

  public abstract ImmutableSet<RelPath> getExcludedPaths();

  @Override
  public String getShortName() {
    return "rm";
  }

  @Override
  public StepExecutionResult executeIsolatedStep(IsolatedExecutionContext context)
      throws IOException {

    AbsPath absolutePath = getAbsPath(context);

    if (isRecursive()) {
      // Delete a folder recursively
      MostFiles.deleteRecursivelyIfExists(absolutePath, getAbsoluteExcludedPaths(context));
    } else {
      // Delete a single file
      Preconditions.checkState(
          getExcludedPaths().isEmpty(), "Excluded paths only valid for recursive steps");
      Files.deleteIfExists(absolutePath.getPath());
    }
    return StepExecutionResults.SUCCESS;
  }

  private AbsPath getAbsPath(IsolatedExecutionContext context) {
    return convertRelPathToAbsPath(context, getPath());
  }

  private AbsPath convertRelPathToAbsPath(IsolatedExecutionContext context, RelPath relPath) {
    return ProjectFilesystemUtils.getAbsPathForRelativePath(context.getRuleCellRoot(), relPath);
  }

  private ImmutableSet<AbsPath> getAbsoluteExcludedPaths(IsolatedExecutionContext context) {
    ImmutableSet<RelPath> excludedRelPaths = getExcludedPaths();
    if (excludedRelPaths.isEmpty()) {
      // Avoid calls to .stream() and creation of new sets as almost all steps will not have
      // any excluded paths
      return ImmutableSet.of();
    }

    return excludedRelPaths.stream()
        .map(relPath -> convertRelPathToAbsPath(context, relPath))
        .collect(ImmutableSet.toImmutableSet());
  }

  @Override
  public String getIsolatedStepDescription(IsolatedExecutionContext context) {
    StringJoiner args = new StringJoiner(" ");
    args.add("rm");
    args.add("-f");

    if (isRecursive()) {
      args.add("-r");
    }

    args.add(getAbsPath(context).toString());

    if (!getExcludedPaths().isEmpty()) {
      args.add("(with excluded paths)");
    }

    return args.toString();
  }

  public static RmIsolatedStep of(
      RelPath path, boolean recursive, ImmutableSet<RelPath> excludedPaths) {
    return ImmutableRmIsolatedStep.ofImpl(path, recursive, excludedPaths);
  }

  public static RmIsolatedStep of(RelPath path, boolean recursive) {
    return of(path, recursive, ImmutableSet.of());
  }

  public static RmIsolatedStep of(RelPath path) {
    return of(path, false);
  }
}
