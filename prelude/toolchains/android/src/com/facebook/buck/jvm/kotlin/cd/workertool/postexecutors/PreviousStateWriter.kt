@file:JvmName("PreviousStateWriterFactory")

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.kotlin.cd.workertool.postexecutors

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardCopyOption

sealed interface PreviousStateWriter {

  fun execute()
}

internal class IncrementalPreviousStateWriter(
    val incrementalStateDir: Path,
    val actionMetadataPath: Path,
) : PreviousStateWriter {

  override fun execute() {
    Files.copy(
        actionMetadataPath,
        incrementalStateDir.resolve(actionMetadataPath.fileName),
        StandardCopyOption.REPLACE_EXISTING,
    )
  }
}

internal data object DoNothingPreviousStateWriter : PreviousStateWriter {

  override fun execute() {
    // do nothing
  }
}

@JvmName("create")
fun PreviousStateWriter(
    shouldActionRunIncrementally: Boolean,
    incrementalStateDir: Path?,
    actionMetadataPath: Path?,
): PreviousStateWriter {
  if (!shouldActionRunIncrementally) {
    return DoNothingPreviousStateWriter
  }

  return IncrementalPreviousStateWriter(
      requireNotNull(incrementalStateDir),
      requireNotNull(actionMetadataPath),
  )
}
