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

import org.junit.Assert.assertNotNull
import org.junit.Assert.assertTrue
import org.junit.Test

class PostExecutorsFactoryTest {

  @Test
  fun `when compiling incrementally, IncrementalPostExecutorsFactory is created`() {
    val writer = PostExecutorsFactory.create(shouldKotlincRunIncrementally = true)

    assertNotNull(writer)
    assertTrue(writer is IncrementalPostExecutorsFactory)
  }

  @Test
  fun `when compiling non incrementally, NonIncrementalPostExecutorsFactory is created`() {
    val writer = PostExecutorsFactory.create(shouldKotlincRunIncrementally = false)

    assertNotNull(writer)
    assertTrue(writer is NonIncrementalPostExecutorsFactory)
  }
}
