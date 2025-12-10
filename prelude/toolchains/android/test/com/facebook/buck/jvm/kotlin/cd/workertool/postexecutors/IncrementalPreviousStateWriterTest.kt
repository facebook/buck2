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
import org.junit.Assert.assertTrue
import org.junit.Before
import org.junit.Rule
import org.junit.Test

internal class IncrementalPreviousStateWriterTest {

  @JvmField @Rule val temporaryFolder: TemporaryPaths = TemporaryPaths(false)

  private lateinit var incrementalStateDir: Path
  private lateinit var actionMetadataPath: Path

  @Before
  fun setup() {
    incrementalStateDir = temporaryFolder.newFolder("incremental-state").path
    actionMetadataPath = temporaryFolder.newFile("action-metadata.json").path
  }

  @Test
  fun `when executed, existing files are copied`() {
    val writer = IncrementalPreviousStateWriter(incrementalStateDir, actionMetadataPath)

    writer.execute()

    assertEquals(1, incrementalStateDir.toFile().listFiles()?.size)
    assertTrue(Files.exists(incrementalStateDir.resolve(actionMetadataPath.fileName)))
  }
}
