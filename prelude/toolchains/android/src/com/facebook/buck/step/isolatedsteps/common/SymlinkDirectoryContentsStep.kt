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
import java.nio.file.Files
import java.nio.file.Path
import kotlin.streams.asSequence

/**
 * Creates symlinks in the destination directory for all files in the source directory. This is
 * similar to CopyIsolatedStep with DIRECTORY_CONTENTS_ONLY mode, but creates symlinks instead of
 * copying files.
 */
class SymlinkDirectoryContentsStep(val sourceDirectory: Path, val destinationDirectory: Path) :
    IsolatedStep {
  override fun getShortName(): String {
    return "symlink_directory_contents"
  }

  @Throws(IOException::class)
  override fun executeIsolatedStep(context: IsolatedExecutionContext): StepExecutionResult {
    val ruleCellRoot = context.ruleCellRoot
    val sourceAbsPath =
        ProjectFilesystemUtils.getAbsPathForRelativePath(ruleCellRoot, sourceDirectory)
    val destAbsPath =
        ProjectFilesystemUtils.getAbsPathForRelativePath(ruleCellRoot, destinationDirectory)

    // Ensure destination directory exists
    Files.createDirectories(destAbsPath.path)

    // Walk through all files in the source directory and create symlinks
    Files.walk(sourceAbsPath.path).use { stream ->
      stream
          .asSequence()
          .filter { Files.isRegularFile(it) }
          .forEach { sourceFile ->
            val relativePath = sourceAbsPath.path.relativize(sourceFile)
            val destFile = destAbsPath.path.resolve(relativePath)

            // Create parent directories if needed
            Files.createDirectories(destFile.parent)

            // Create symlink
            ProjectFilesystemUtils.createSymLink(
                ruleCellRoot,
                destFile,
                sourceFile,
                /* force */ true,
            )
          }
    }

    return StepExecutionResults.SUCCESS
  }

  override fun getIsolatedStepDescription(context: IsolatedExecutionContext): String {
    return "ln -s $sourceDirectory/* $destinationDirectory/"
  }

  companion object {
    @JvmStatic
    fun of(sourceDirectory: RelPath, destinationDirectory: RelPath): SymlinkDirectoryContentsStep {
      return SymlinkDirectoryContentsStep(sourceDirectory.path, destinationDirectory.path)
    }
  }
}
