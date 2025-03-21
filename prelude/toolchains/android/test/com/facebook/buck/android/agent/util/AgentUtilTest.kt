/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android.agent.util

import com.facebook.buck.testutil.integration.TestDataHelper
import org.junit.Assert.assertEquals
import org.junit.Test

class AgentUtilTest {
  @Test
  fun testGetJarSignature() {
    val testDataDir = TestDataHelper.getTestDataDirectory(this)
    val testJar = testDataDir.resolve("example.jar")
    val jarSignature = AgentUtil.getJarSignature(testJar.toAbsolutePath().toString())
    assertEquals("JB2+Jt7N6wdguWfvzaM3cJiisTM=", jarSignature)
  }
}
