/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.step.isolatedsteps.common

import com.facebook.buck.core.build.execution.context.IsolatedExecutionContext
import com.facebook.buck.core.filesystems.RelPath
import com.facebook.buck.io.filesystem.impl.ProjectFilesystemUtils
import com.facebook.buck.step.StepExecutionResult
import com.facebook.buck.step.StepExecutionResults
import com.facebook.buck.step.isolatedsteps.IsolatedStep
import java.io.IOException

/** Creates a symlink from a desired path to an existing path. */
data class SymlinkIsolatedStep(val existingPath: RelPath, val desiredPath: RelPath) : IsolatedStep {
  override fun getShortName(): String {
    return "symlink_file"
  }

  @Throws(IOException::class)
  override fun executeIsolatedStep(context: IsolatedExecutionContext): StepExecutionResult {
    val ruleCellRoot = context.ruleCellRoot

    val existingAbsPath =
        ProjectFilesystemUtils.getAbsPathForRelativePath(ruleCellRoot, existingPath)
    val desiredAbsPath = ProjectFilesystemUtils.getAbsPathForRelativePath(ruleCellRoot, desiredPath)

    ProjectFilesystemUtils.createSymLink(
        ruleCellRoot, desiredAbsPath.path, existingAbsPath.path, /* force */ true)

    return StepExecutionResults.SUCCESS
  }

  override fun getIsolatedStepDescription(context: IsolatedExecutionContext): String {
    val ruleCellRoot = context.ruleCellRoot
    return java.lang.String.join(
        " ",
        "ln",
        "-f",
        "-s",
        ProjectFilesystemUtils.getAbsPathForRelativePath(ruleCellRoot, existingPath).toString(),
        ProjectFilesystemUtils.getAbsPathForRelativePath(ruleCellRoot, desiredPath).toString())
  }
}
