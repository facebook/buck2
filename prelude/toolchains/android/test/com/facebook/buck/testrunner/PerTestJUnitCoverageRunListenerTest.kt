/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.testrunner

import java.io.File
import org.junit.Assert.assertTrue
import org.junit.Assert.fail
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TemporaryFolder
import org.junit.runner.Description

/**
 * Tests for [PerTestJUnitCoverageRunListener]-specific behavior — the per-run discriminator that
 * keeps `_setup`/`_teardown` filenames unique across the multiple `jUnitCore.run()` invocations
 * that `JUnitRunner` issues per test class, and the [ReflectiveCoverageAgent] default constructor
 * behavior when the JaCoCo runtime agent is not on the classpath. The shared lifecycle is tested in
 * [BasePerTestCoverageRunListenerTest].
 */
class PerTestJUnitCoverageRunListenerTest {

  @get:Rule val tempDir = TemporaryFolder()

  @Test
  fun `setup and teardown include run count suffix`() {
    val outputDir = tempDir.newFolder("coverage")
    val agent = FakeCoverageAgent(byteArrayOf(1))
    val listener = PerTestJUnitCoverageRunListener(outputDir, agent)

    val desc = Description.createSuiteDescription("classes")
    listener.testRunStarted(desc)
    listener.testRunFinished(null)
    listener.close()

    assertTrue(
        "Setup includes run discriminator",
        File(outputDir, "_setup_classes_1.exec").exists(),
    )
    assertTrue(
        "Teardown includes run discriminator",
        File(outputDir, "_teardown_classes_1.exec").exists(),
    )
  }

  @Test
  fun `run count increments across multiple runs`() {
    // Simulates the JUnitRunner pattern: jUnitCore.run() invoked once per test class, with the
    // same listener instance reused. Without the run counter, the second run's setup/teardown
    // dumps would overwrite the first.
    val outputDir = tempDir.newFolder("coverage")
    val agent = FakeCoverageAgent(byteArrayOf(1))
    val listener = PerTestJUnitCoverageRunListener(outputDir, agent)

    val desc = Description.createSuiteDescription("classes")
    listener.testRunStarted(desc)
    listener.testRunFinished(null)
    listener.testRunStarted(desc)
    listener.testRunFinished(null)
    listener.close()

    assertTrue(File(outputDir, "_setup_classes_1.exec").exists())
    assertTrue(File(outputDir, "_teardown_classes_1.exec").exists())
    assertTrue(File(outputDir, "_setup_classes_2.exec").exists())
    assertTrue(File(outputDir, "_teardown_classes_2.exec").exists())
  }

  @Test
  fun `default constructor throws without JaCoCo agent`() {
    val outputDir = tempDir.newFolder("coverage")
    try {
      PerTestJUnitCoverageRunListener(outputDir)
      fail("Expected ClassNotFoundException")
    } catch (e: ClassNotFoundException) {
      // Expected: JaCoCo agent is not on the test classpath
    }
  }

  /** Fake [BasePerTestCoverageRunListener.CoverageAgent] returning fixed data. */
  private class FakeCoverageAgent(private val execData: ByteArray?) :
      BasePerTestCoverageRunListener.CoverageAgent {
    override fun reset() {}

    override fun dumpAndReset(): ByteArray? = execData
  }
}
