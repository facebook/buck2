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
import java.io.BufferedWriter
import java.io.Closeable
import java.io.File
import java.io.FileOutputStream
import java.io.FileWriter
import java.io.PrintWriter
import java.io.StringWriter
import org.junit.runner.Description
import org.junit.runner.notification.RunListener

/**
 * A RunListener that collects per-test-method JaCoCo coverage data in bundled mode.
 *
 * When tests run in bundled mode (run_as_bundle), JaCoCo produces a single combined .exec file for
 * all test methods. This listener uses JaCoCo's runtime agent API to reset coverage probes before
 * each test and dump execution data after each test, producing per-test-method .exec files.
 *
 * The JaCoCo agent API is accessed via reflection to avoid compile-time dependencies. If the JaCoCo
 * agent is not present (non-coverage builds), initialization fails gracefully and no coverage data
 * is collected.
 *
 * Output:
 * - {outputDir}/{sanitized_test_name}.exec - Binary JaCoCo execution data per test method
 * - {outputDir}/manifest.jsonl - JSONL mapping test names to .exec file paths
 *
 * Test names are emitted in TPX format ("methodName (className)") so they match the test names the
 * TPX result reporter uses to attach per-test data.
 *
 * Note: sanitizeFileName replaces characters invalid in filenames with underscores. Test names that
 * differ only in these characters (e.g., special chars like <, *, ") would map to the same
 * filename. This is unlikely in practice but worth noting for parameterized tests with unusual
 * names.
 *
 * Activation: Set the PER_TEST_COVERAGE_DIR environment variable to the output directory path.
 */
class PerTestJUnitCoverageRunListener
@JvmOverloads
constructor(
    private val outputDir: File,
    private val agent: CoverageAgent = ReflectiveCoverageAgent(),
) : RunListener(), Closeable {

  /** Abstraction over JaCoCo's runtime agent for testability. */
  interface CoverageAgent {
    fun reset()

    fun getExecutionData(reset: Boolean): ByteArray?
  }

  /** Default implementation that calls JaCoCo's RT.getAgent() via reflection. */
  class ReflectiveCoverageAgent : CoverageAgent {
    private val jacocoAgent: Any
    private val resetMethod: java.lang.reflect.Method
    private val getExecDataMethod: java.lang.reflect.Method

    init {
      val rtClass = Class.forName("org.jacoco.agent.rt.RT")
      jacocoAgent =
          rtClass.getMethod("getAgent").invoke(null)
              ?: throw IllegalStateException("JaCoCo agent returned null from RT.getAgent()")
      resetMethod = jacocoAgent::class.java.getMethod("reset")
      getExecDataMethod =
          jacocoAgent::class.java.getMethod("getExecutionData", Boolean::class.javaPrimitiveType)
    }

    override fun reset() {
      resetMethod.invoke(jacocoAgent)
    }

    override fun getExecutionData(reset: Boolean): ByteArray? {
      return getExecDataMethod.invoke(jacocoAgent, reset) as? ByteArray
    }
  }

  /** First coverage error encountered, if any. Checked by JUnitRunner after test execution. */
  var coverageError: Exception? = null
    private set

  private val manifest: PrintWriter
  private val jsonFactory = JsonFactory()

  init {
    check(outputDir.mkdirs() || outputDir.isDirectory) {
      "Failed to create per-test coverage output directory: $outputDir"
    }
    manifest = PrintWriter(BufferedWriter(FileWriter(File(outputDir, "manifest.jsonl"))), true)
  }

  // Per-testRun discriminator. JUnitRunner reuses one listener across multiple
  // jUnitCore.run() invocations (one per test class), and the suite's `description.displayName`
  // is the JUnit default "classes" for every invocation — so we tack on a counter to guarantee
  // unique setup/teardown exec filenames per run, otherwise they overwrite each other.
  private var runCount = 0
  private var currentRunSuffix: String = "anon"

  override fun testRunStarted(description: Description?) {
    runCount += 1
    val base = sanitizeFileName(description?.displayName ?: "anon")
    currentRunSuffix = "${base}_$runCount"
    // Snapshot probes accumulated since JVM start (test framework boot, class loading,
    // static initializers). These are bundle-level — no individual @Test caused them —
    // so they're attributed to the "main" test row TPX synthesizes per bundle. Without
    // this, the next testStarted's reset() would wipe them silently.
    dumpExec(testName = "main", baseFileName = "_setup_$currentRunSuffix")
  }

  override fun testStarted(description: Description) {
    try {
      agent.reset()
    } catch (e: Exception) {
      coverageError = coverageError ?: e
    }
  }

  override fun testFinished(description: Description) {
    val className = description.className ?: return
    val methodName = description.methodName ?: return
    // TPX-format test name: matches `r.name` produced by the JUnit standard output listener
    // (`JUnitTpxStandardOutputListener.getFullTestName`), so the TPX result reporter can
    // attach this entry to the right test row without a translation step.
    val testName = "$methodName ($className)"
    dumpExec(testName = testName, baseFileName = testName)
  }

  override fun testRunFinished(result: org.junit.runner.Result?) {
    // Probes that fire AFTER the last test's testFinished (framework teardown, deferred
    // class loading) — same bundle-level attribution as setup.
    // Do NOT close() here: JUnitRunner reuses one listener instance across multiple
    // jUnitCore.run() invocations (one per test class), so testRunFinished fires per class.
    // Closing the manifest writer after the first class would silently drop later writes.
    // The owner (JUnitRunner) calls close() once after all classes are done.
    dumpExec(testName = "main", baseFileName = "_teardown_$currentRunSuffix")
  }

  override fun close() {
    manifest.close()
  }

  private fun dumpExec(testName: String, baseFileName: String) {
    try {
      val execData = agent.getExecutionData(true) ?: return
      if (execData.isEmpty()) return

      val fileName = sanitizeFileName(baseFileName) + ".exec"
      val execFile = File(outputDir, fileName)
      FileOutputStream(execFile).use { fos -> fos.write(execData) }

      val jsonLine =
          StringWriter()
              .also { sw ->
                jsonFactory.createGenerator(sw).use { gen ->
                  gen.writeStartObject()
                  gen.writeStringField("test_name", testName)
                  gen.writeStringField("exec_file", fileName)
                  gen.writeEndObject()
                }
              }
              .toString()

      synchronized(manifest) { manifest.println(jsonLine) }
    } catch (e: Exception) {
      coverageError = coverageError ?: e
    }
  }

  companion object {
    private val INVALID_FILENAME_CHARS = Regex("[/\\\\:*?\"<>| ]")

    internal fun sanitizeFileName(name: String): String = name.replace(INVALID_FILENAME_CHARS, "_")
  }
}
