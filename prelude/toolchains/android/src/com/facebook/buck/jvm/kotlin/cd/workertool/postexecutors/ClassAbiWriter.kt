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
import com.facebook.buck.jvm.java.abi.StubJar

sealed interface ClassAbiWriter {

  fun execute()
}

internal data class NonIncrementalAbiWriter(
    private val libraryStubJar: StubJar,
    private val jvmAbiGenJar: AbsPath,
    private val abiJar: AbsPath
) : ClassAbiWriter {

  constructor(
      libraryJar: AbsPath,
      jvmAbiGenJar: AbsPath,
      abiJar: AbsPath
  ) : this(StubJar(libraryJar), jvmAbiGenJar, abiJar)

  override fun execute() {
    check(jvmAbiGenJar.toFile().exists())

    libraryStubJar.apply {
      setExistingAbiJar(jvmAbiGenJar)
      writeTo(abiJar)
    }
  }
}

internal data class IncrementalAbiWriter(
    private val kotlincOutputStubJar: StubJar,
    private val jvmAbiGenWorkingDir: AbsPath,
    private val jvmAbiGenJar: AbsPath,
    private val libraryStubJar: StubJar,
    private val abiJar: AbsPath
) : ClassAbiWriter {

  constructor(
      kotlincOutputDir: AbsPath,
      jvmAbiGenWorkingDir: AbsPath,
      jvmAbiGenJar: AbsPath,
      libraryJar: AbsPath,
      abiJar: AbsPath
  ) : this(
      StubJar(kotlincOutputDir), jvmAbiGenWorkingDir, jvmAbiGenJar, StubJar(libraryJar), abiJar)

  override fun execute() {
    kotlincOutputStubJar.apply {
      setExistingAbiJar(jvmAbiGenWorkingDir)
      writeTo(jvmAbiGenJar)
    }

    libraryStubJar.apply {
      setExistingAbiJar(jvmAbiGenJar)
      writeTo(abiJar)
    }
  }
}
