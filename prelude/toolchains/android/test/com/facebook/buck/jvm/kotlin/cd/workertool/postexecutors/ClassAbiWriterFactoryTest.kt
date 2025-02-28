/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.kotlin.cd.workertool.postexecutors

import java.nio.file.Paths
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertTrue
import org.junit.Test

internal class ClassAbiWriterFactoryTest {

  @Test
  fun `when compiling incrementally, IncrementalAbiWriter is created`() {
    val writer =
        ClassAbiWriter.create(
            shouldKotlincRunIncrementally = true,
            abiJar = Paths.get("abi.jar"),
            libraryJar = Paths.get("library.jar"),
            jvmAbiGenOutput = Paths.get("jvm_abi_gen_output"))

    assertNotNull(writer)
    assertTrue(writer is IncrementalAbiWriter)
  }

  @Test
  fun `when compiling non incrementally, NonIncrementalAbiWriter is created`() {
    val writer =
        ClassAbiWriter.create(
            shouldKotlincRunIncrementally = false,
            abiJar = Paths.get("abi.jar"),
            libraryJar = Paths.get("library.jar"),
            jvmAbiGenOutput = Paths.get("jvm_abi_gen_output"))

    assertNotNull(writer)
    assertTrue(writer is NonIncrementalAbiWriter)
  }
}
