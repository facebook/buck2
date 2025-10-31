// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

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
import com.facebook.buck.testutil.TemporaryPaths
import org.junit.Before
import org.junit.Rule
import org.junit.Test
import org.mockito.kotlin.mock
import org.mockito.kotlin.verify

internal class ClassAbiWriterTest {

  @JvmField @Rule val temporaryFolder: TemporaryPaths = TemporaryPaths(false)

  private val libraryStubJar = mock<StubJar>()

  private lateinit var libraryJar: AbsPath
  private lateinit var jvmAbiGenDir: AbsPath
  private lateinit var abiJar: AbsPath

  @Before
  fun setUp() {
    libraryJar = temporaryFolder.newFile("library.jar")
    abiJar = temporaryFolder.newFile("abi.jar")
    jvmAbiGenDir = temporaryFolder.newFile("jvm_abi_gen_output")
  }

  @Test
  fun `when jvmAbiGenOutput exists, abi jar is created`() {
    val writer = ClassAbiWriter(libraryStubJar, jvmAbiGenDir, abiJar)

    writer.execute()

    verify(libraryStubJar).setExistingAbiJar(jvmAbiGenDir)
    verify(libraryStubJar).writeTo(abiJar)
  }
}
