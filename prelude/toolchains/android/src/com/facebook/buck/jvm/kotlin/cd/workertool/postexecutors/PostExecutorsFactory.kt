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

import com.facebook.buck.core.filesystems.AbsPath
import java.nio.file.Path
import java.nio.file.Paths

sealed interface PostExecutorsFactory {

  fun createClassAbiWriter(
      kotlincOutputDir: AbsPath?,
      jvmAbiGenWorkingDir: AbsPath?,
      jvmAbiGenJar: Path?,
      libraryJar: Path?,
      abiJar: Path?
  ): ClassAbiWriter

  fun createPreviousStateWriter(
      incrementalStateDir: Path?,
      actionMetadataPath: Path?,
      usedClassesPaths: List<Path>
  ): PreviousStateWriter

  companion object {
    @JvmStatic
    fun create(shouldKotlincRunIncrementally: Boolean): PostExecutorsFactory =
        if (shouldKotlincRunIncrementally) {
          IncrementalPostExecutorsFactory
        } else {
          NonIncrementalPostExecutorsFactory
        }
  }
}

internal data object IncrementalPostExecutorsFactory : PostExecutorsFactory {

  private val root: AbsPath = AbsPath.of(Paths.get(".").toAbsolutePath().normalize())

  override fun createClassAbiWriter(
      kotlincOutputDir: AbsPath?,
      jvmAbiGenWorkingDir: AbsPath?,
      jvmAbiGenJar: Path?,
      libraryJar: Path?,
      abiJar: Path?
  ): IncrementalAbiWriter {
    return IncrementalAbiWriter(
        kotlincOutputDir = requireNotNull(kotlincOutputDir),
        jvmAbiGenWorkingDir = requireNotNull(jvmAbiGenWorkingDir),
        jvmAbiGenJar = root.resolve(requireNotNull(jvmAbiGenJar)),
        libraryJar = root.resolve(requireNotNull(libraryJar)),
        abiJar = root.resolve(requireNotNull(abiJar)))
  }

  override fun createPreviousStateWriter(
      incrementalStateDir: Path?,
      actionMetadataPath: Path?,
      usedClassesPaths: List<Path>
  ): IncrementalPreviousStateWriter {
    return IncrementalPreviousStateWriter(
        requireNotNull(incrementalStateDir), requireNotNull(actionMetadataPath), usedClassesPaths)
  }
}

internal data object NonIncrementalPostExecutorsFactory : PostExecutorsFactory {

  private val root: AbsPath = AbsPath.of(Paths.get(".").toAbsolutePath().normalize())

  override fun createClassAbiWriter(
      kotlincOutputDir: AbsPath?,
      jvmAbiGenWorkingDir: AbsPath?,
      jvmAbiGenJar: Path?,
      libraryJar: Path?,
      abiJar: Path?
  ): NonIncrementalAbiWriter {
    return NonIncrementalAbiWriter(
        libraryJar = root.resolve(requireNotNull(libraryJar)),
        jvmAbiGenJar = root.resolve(requireNotNull(jvmAbiGenJar)),
        abiJar = root.resolve(requireNotNull(abiJar)))
  }

  override fun createPreviousStateWriter(
      incrementalStateDir: Path?,
      actionMetadataPath: Path?,
      usedClassesPaths: List<Path>
  ): DoNothingPreviousStateWriter {
    return DoNothingPreviousStateWriter
  }
}
