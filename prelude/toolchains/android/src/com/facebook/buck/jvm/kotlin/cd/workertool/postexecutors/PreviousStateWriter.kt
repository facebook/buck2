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

internal data class IncrementalPreviousStateWriter(
    private val incrementalStateDir: Path,
    private val actionMetadataPath: Path,
    private val usedClassesPaths: List<Path>,
) : PreviousStateWriter {

  override fun execute() {
    val state = buildList {
      add(actionMetadataPath)
      addAll(usedClassesPaths.filter { path -> Files.exists(path) })
    }

    state.forEach { path ->
      Files.copy(
          path, incrementalStateDir.resolve(path.fileName), StandardCopyOption.REPLACE_EXISTING)
    }
  }
}

internal data object DoNothingPreviousStateWriter : PreviousStateWriter {

  override fun execute() {
    // do nothing
  }
}
