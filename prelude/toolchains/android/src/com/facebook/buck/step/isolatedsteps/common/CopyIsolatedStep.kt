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
import com.facebook.buck.io.filesystem.CopySourceMode
import com.facebook.buck.io.filesystem.impl.ProjectFilesystemUtils
import com.facebook.buck.step.StepExecutionResult
import com.facebook.buck.step.StepExecutionResults
import com.facebook.buck.step.isolatedsteps.IsolatedStep
import java.io.File
import java.io.IOException
import java.nio.file.Path
import java.util.StringJoiner

/** Copy IsolatedStep */
data class CopyIsolatedStep(
    val source: Path,
    val destination: Path,
    val copySourceMode: CopySourceMode
) : IsolatedStep {
  override fun getShortName(): String {
    return "cp"
  }

  override fun getIsolatedStepDescription(context: IsolatedExecutionContext): String {
    val args = StringJoiner(" ")
    args.add("cp")
    when (copySourceMode) {
      CopySourceMode.FILE -> args.add(source.toString())
      CopySourceMode.DIRECTORY_AND_CONTENTS -> {
        args.add("-R")
        args.add(source.toString())
      }
      CopySourceMode.DIRECTORY_CONTENTS_ONLY -> {
        args.add("-R")

        // BSD and GNU cp have different behaviors with -R:
        // http://jondavidjohn.com/blog/2012/09/linux-vs-osx-the-cp-command
        //
        // To work around this, we use "sourceDir/*" as the source to
        // copy in this mode.

        // N.B., on Windows, java.nio.AbstractPath does not resolve *
        // as a path, causing InvalidPathException. Since this is purely a
        // description, manually create the source argument.
        args.add(source.toString() + File.separator + "*")
      }
    }
    args.add(destination.toString())
    return args.toString()
  }

  @Throws(IOException::class)
  override fun executeIsolatedStep(context: IsolatedExecutionContext): StepExecutionResult {
    ProjectFilesystemUtils.copy(context.ruleCellRoot, source, destination, copySourceMode)
    return StepExecutionResults.SUCCESS
  }

  companion object {
    @JvmStatic
    fun of(source: Path, destination: Path, copySourceMode: CopySourceMode): CopyIsolatedStep {
      return CopyIsolatedStep(source, destination, copySourceMode)
    }

    /** Creates a CopyStep which copies a single file from 'source' to 'destination'. */
    fun forFile(source: Path, destination: Path): CopyIsolatedStep {
      return CopyIsolatedStep(source, destination, CopySourceMode.FILE)
    }

    /** Creates a CopyStep which copies a single file from 'source' to 'destination'. */
    fun forFile(source: RelPath, destination: RelPath): CopyIsolatedStep {
      return forFile(source.path, destination.path)
    }

    /** Creates a CopyStep which recursively copies a directory from 'source' to 'destination'. */
    @JvmStatic
    fun forDirectory(
        source: Path,
        destination: Path,
        copySourceMode: CopySourceMode
    ): CopyIsolatedStep {
      return CopyIsolatedStep(source, destination, copySourceMode)
    }

    /** Creates a CopyStep which recursively copies a directory from 'source' to 'destination'. */
    @JvmStatic
    fun forDirectory(
        source: RelPath,
        destination: RelPath,
        copySourceMode: CopySourceMode
    ): CopyIsolatedStep {
      return forDirectory(source.path, destination.path, copySourceMode)
    }
  }
}
