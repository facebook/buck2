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

import com.fasterxml.jackson.core.JsonFactory
import java.io.File
import org.junit.Assert.assertArrayEquals
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TemporaryFolder
import org.junit.runner.Description

class PerTestJUnitCoverageRunListenerTest {

  @get:Rule val tempDir = TemporaryFolder()

  @Test
  fun testWritesExecFilePerTest() {
    val outputDir = tempDir.newFolder("coverage")
    val agent = FakeCoverageAgent(byteArrayOf(1, 2, 3))
    val listener = PerTestJUnitCoverageRunListener(outputDir, agent)

    val desc = Description.createTestDescription("com.example.FooTest", "testBar")
    listener.testStarted(desc)
    listener.testFinished(desc)
    listener.close()

    val execFile = File(outputDir, "testBar_(com.example.FooTest).exec")
    assertTrue("Exec file should be created", execFile.exists())
    assertArrayEquals(byteArrayOf(1, 2, 3), execFile.readBytes())
  }

  @Test
  fun testWritesManifestJsonl() {
    val outputDir = tempDir.newFolder("coverage")
    val agent = FakeCoverageAgent(byteArrayOf(1))
    val listener = PerTestJUnitCoverageRunListener(outputDir, agent)

    val desc = Description.createTestDescription("com.example.FooTest", "testBar")
    listener.testStarted(desc)
    listener.testFinished(desc)
    listener.close()

    val manifest = File(outputDir, "manifest.jsonl")
    assertTrue("Manifest should exist", manifest.exists())

    val line = manifest.readLines().single()
    val parser = JsonFactory().createParser(line)
    parser.nextToken() // START_OBJECT
    parser.nextToken() // FIELD_NAME
    assertEquals("test_name", parser.currentName())
    parser.nextToken()
    assertEquals("testBar (com.example.FooTest)", parser.text)
    parser.nextToken() // FIELD_NAME
    assertEquals("exec_file", parser.currentName())
    parser.nextToken()
    assertEquals("testBar_(com.example.FooTest).exec", parser.text)
  }

  @Test
  fun testResetsAgentBeforeEachTest() {
    val outputDir = tempDir.newFolder("coverage")
    val agent = FakeCoverageAgent(byteArrayOf(1))
    val listener = PerTestJUnitCoverageRunListener(outputDir, agent)

    val desc1 = Description.createTestDescription("com.example.FooTest", "test1")
    val desc2 = Description.createTestDescription("com.example.FooTest", "test2")
    listener.testStarted(desc1)
    listener.testFinished(desc1)
    listener.testStarted(desc2)
    listener.testFinished(desc2)
    listener.close()

    assertEquals("Agent should be reset before each test", 2, agent.resetCount)
  }

  @Test
  fun testSkipsEmptyExecData() {
    val outputDir = tempDir.newFolder("coverage")
    val agent = FakeCoverageAgent(byteArrayOf()) // empty
    val listener = PerTestJUnitCoverageRunListener(outputDir, agent)

    val desc = Description.createTestDescription("com.example.FooTest", "testEmpty")
    listener.testStarted(desc)
    listener.testFinished(desc)
    listener.close()

    assertEquals(
        "No exec files for empty coverage data",
        0,
        outputDir.listFiles { f -> f.name.endsWith(".exec") }?.size ?: 0,
    )
  }

  @Test
  fun testSkipsNullExecData() {
    val outputDir = tempDir.newFolder("coverage")
    val agent = FakeCoverageAgent(null)
    val listener = PerTestJUnitCoverageRunListener(outputDir, agent)

    val desc = Description.createTestDescription("com.example.FooTest", "testNull")
    listener.testStarted(desc)
    listener.testFinished(desc)
    listener.close()

    assertEquals(
        "No exec files for null coverage data",
        0,
        outputDir.listFiles { f -> f.name.endsWith(".exec") }?.size ?: 0,
    )
  }

  @Test
  fun testSanitizesSpecialCharactersInFileName() {
    val outputDir = tempDir.newFolder("coverage")
    val agent = FakeCoverageAgent(byteArrayOf(1))
    val listener = PerTestJUnitCoverageRunListener(outputDir, agent)

    // Parameterized tests can have spaces and special chars
    val desc = Description.createTestDescription("com.example.FooTest", "test[param1, param2]")
    listener.testStarted(desc)
    listener.testFinished(desc)
    listener.close()

    val execFiles = outputDir.listFiles { f -> f.name.endsWith(".exec") }!!
    assertEquals(1, execFiles.size)
    assertEquals("test[param1,_param2]_(com.example.FooTest).exec", execFiles[0].name)
  }

  @Test
  fun testResetFailureCapturedInCoverageError() {
    val outputDir = tempDir.newFolder("coverage")
    val agent =
        object : PerTestJUnitCoverageRunListener.CoverageAgent {
          override fun reset() {
            throw RuntimeException("agent unavailable")
          }

          override fun getExecutionData(reset: Boolean): ByteArray? = byteArrayOf(1)
        }
    val listener = PerTestJUnitCoverageRunListener(outputDir, agent)

    val desc = Description.createTestDescription("com.example.FooTest", "testBar")
    listener.testStarted(desc)
    listener.testFinished(desc)
    listener.close()

    assertEquals("agent unavailable", listener.coverageError?.message)
  }

  @Test
  fun testDumpFailureCapturedInCoverageError() {
    val outputDir = tempDir.newFolder("coverage")
    val agent =
        object : PerTestJUnitCoverageRunListener.CoverageAgent {
          override fun reset() {}

          override fun getExecutionData(reset: Boolean): ByteArray? {
            throw RuntimeException("dump failed")
          }
        }
    val listener = PerTestJUnitCoverageRunListener(outputDir, agent)

    val desc = Description.createTestDescription("com.example.FooTest", "testBar")
    listener.testStarted(desc)
    listener.testFinished(desc)
    listener.close()

    assertEquals("dump failed", listener.coverageError?.message)
    assertEquals(0, outputDir.listFiles { f -> f.name.endsWith(".exec") }?.size ?: 0)
  }

  @Test
  fun testNoCoverageErrorOnSuccess() {
    val outputDir = tempDir.newFolder("coverage")
    val agent = FakeCoverageAgent(byteArrayOf(1))
    val listener = PerTestJUnitCoverageRunListener(outputDir, agent)

    val desc = Description.createTestDescription("com.example.FooTest", "testBar")
    listener.testStarted(desc)
    listener.testFinished(desc)
    listener.close()

    assertEquals(null, listener.coverageError)
  }

  @Test
  fun testDefaultConstructorThrowsWithoutJacocoAgent() {
    val outputDir = tempDir.newFolder("coverage")
    try {
      PerTestJUnitCoverageRunListener(outputDir)
      org.junit.Assert.fail("Expected ClassNotFoundException")
    } catch (e: ClassNotFoundException) {
      // Expected: JaCoCo agent is not on the test classpath
    }
  }

  /**
   * A fake [PerTestJUnitCoverageRunListener.CoverageAgent] that returns fixed data and tracks reset
   * calls, similar to how [TpxTimeoutBufferManagerTest.ManualScheduler] tracks scheduled tasks.
   */
  private class FakeCoverageAgent(private val execData: ByteArray?) :
      PerTestJUnitCoverageRunListener.CoverageAgent {
    var resetCount = 0
      private set

    override fun reset() {
      resetCount++
    }

    override fun getExecutionData(reset: Boolean): ByteArray? = execData
  }
}
