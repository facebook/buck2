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

import com.facebook.buck.testutil.TemporaryPaths
import java.nio.file.Files
import java.nio.file.Path
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Before
import org.junit.Rule
import org.junit.Test

internal class IncrementalPreviousStateWriterTest {

  @JvmField @Rule val temporaryFolder: TemporaryPaths = TemporaryPaths(false)

  private lateinit var incrementalStateDir: Path
  private lateinit var actionMetadataPath: Path
  private lateinit var usedClassesPaths: List<Path>

  @Before
  fun setup() {
    incrementalStateDir = temporaryFolder.newFolder("incremental-state").path
    actionMetadataPath = temporaryFolder.newFile("action-metadata.json").path
    usedClassesPaths =
        listOf(
            temporaryFolder.newFile("kotlin_used_classes.json").path,
            temporaryFolder.newFile("used_classes.json").path,
            temporaryFolder.root.resolve("non_existing_used_classes.json").path)
  }

  @Test
  fun `when executed, existing files are copied`() {
    val writer =
        IncrementalPreviousStateWriter(incrementalStateDir, actionMetadataPath, usedClassesPaths)

    writer.execute()

    assertEquals(3, incrementalStateDir.toFile().listFiles()?.size)
    assertTrue(Files.exists(incrementalStateDir.resolve(actionMetadataPath.fileName)))
    assertTrue(Files.exists(incrementalStateDir.resolve(usedClassesPaths[0].fileName)))
    assertTrue(Files.exists(incrementalStateDir.resolve(usedClassesPaths[1].fileName)))
    assertFalse(Files.exists(incrementalStateDir.resolve(usedClassesPaths[2].fileName)))
  }
}
