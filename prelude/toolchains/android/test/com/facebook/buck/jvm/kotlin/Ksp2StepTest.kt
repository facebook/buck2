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

import com.facebook.buck.jvm.kotlin.ksp.Ksp2Step
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class Ksp2StepTest {

  @Test
  fun `records counts for successful runs`() {
    assertTrue(Ksp2Step.shouldRecordProcessorCounts(true, true))
  }

  @Test
  fun `skips counts on failure`() {
    assertFalse(Ksp2Step.shouldRecordProcessorCounts(false, true))
  }

  @Test
  fun `skips counts when empty`() {
    assertFalse(Ksp2Step.shouldRecordProcessorCounts(true, false))
  }
}
