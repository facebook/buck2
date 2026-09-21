/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.kotlin

import com.facebook.buck.core.filesystems.RelPath
import com.facebook.buck.jvm.kotlin.ksp.Ksp2Step
import com.facebook.buck.jvm.kotlin.ksp.incremental.Ksp2Mode
import com.facebook.buck.testutil.TemporaryPaths
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Rule
import org.junit.Test

class Ksp2StepTest {

  @Rule @JvmField val temporaryPaths = TemporaryPaths()

  private val nonIncremental = Ksp2Mode.NonIncremental(RelPath.get("kspCaches"))

  private fun incremental() =
      Ksp2Mode.Incremental(
          cachesDir = temporaryPaths.root.resolve("caches"),
          incrementalLog = false,
          modifiedSources = emptyList(),
          removedSources = emptyList(),
          changedClasses = emptyList(),
          reprocessReason = null,
      )

  @Test
  fun `records counts for successful non-incremental runs`() {
    assertTrue(Ksp2Step.shouldRecordProcessorCounts(true, true, nonIncremental))
  }

  @Test
  fun `skips counts for incremental runs`() {
    assertFalse(Ksp2Step.shouldRecordProcessorCounts(true, true, incremental()))
  }

  @Test
  fun `skips counts on failure`() {
    assertFalse(Ksp2Step.shouldRecordProcessorCounts(false, true, nonIncremental))
  }

  @Test
  fun `skips counts when empty`() {
    assertFalse(Ksp2Step.shouldRecordProcessorCounts(true, false, nonIncremental))
  }
}
