/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.testrunner;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class CrashAnalyzerTest {

  private CrashAnalyzer crashAnalyzer;
  private ByteArrayOutputStream errContent;
  private PrintStream originalErr;

  @Before
  public void setUp() {
    crashAnalyzer = new CrashAnalyzer();
    errContent = new ByteArrayOutputStream();
    originalErr = System.err;
    System.setErr(new PrintStream(errContent));
  }

  @After
  public void tearDown() {
    System.setErr(originalErr);
  }

  @Test
  public void testDetectsOutOfMemoryError() {
    String logcatOutput =
        "E/AndroidRuntime(12345): FATAL EXCEPTION: main\n"
            + "E/AndroidRuntime(12345): java.lang.OutOfMemoryError: Failed to allocate 100MB\n"
            + "E/AndroidRuntime(12345): \tat"
            + " com.facebook.example.SomeClass.method(SomeClass.java:42)\n"
            + "E/AndroidRuntime(12345): \tat"
            + " com.facebook.example.MainActivity.onCreate(MainActivity.java:123)\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue("Should detect OOM error", errorOutput.contains("Out of memory error detected"));
    assertTrue(
        "Should print crash analysis header", errorOutput.contains("=== AIT CRASH ANALYSIS ==="));
    assertTrue(
        "Should include OOM crash line",
        errorOutput.contains("java.lang.OutOfMemoryError: Failed to allocate 100MB"));
    assertTrue("Should include stack trace", errorOutput.contains("at com.facebook.example"));
  }

  @Test
  public void testDetectsOutOfMemoryErrorWithDifferentMessage() {
    String logcatOutput =
        "E/AndroidRuntime(67890): java.lang.OutOfMemoryError: pthread_create (1040KB stack)"
            + " failed\n"
            + "E/AndroidRuntime(67890): \tat java.lang.Thread.nativeCreate(Native Method)\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue("Should detect OOM error", errorOutput.contains("Out of memory error detected"));
    assertTrue("Should include pthread_create OOM message", errorOutput.contains("pthread_create"));
  }

  @Test
  public void testNoDetectionForNormalLogcat() {
    String logcatOutput =
        "I/ActivityManager(1234): Start proc com.facebook.example\n"
            + "D/MainActivity(5678): onCreate called\n"
            + "I/TestRunner(9012): Test passed successfully\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should always print analysis header", errorOutput.contains("=== AIT CRASH ANALYSIS ==="));
    assertTrue("Should print no crashes detected", errorOutput.contains("No crashes detected"));
    assertFalse(
        "Should not report OOM for normal logs",
        errorOutput.contains("Out of memory error detected"));
  }

  @Test
  public void testHandlesNullInput() {
    crashAnalyzer.analyzeCrashInformation(null);

    String errorOutput = errContent.toString();
    assertTrue("Should produce no output for null input", errorOutput.isEmpty());
  }

  @Test
  public void testHandlesEmptyInput() {
    crashAnalyzer.analyzeCrashInformation("");

    String errorOutput = errContent.toString();
    assertTrue("Should produce no output for empty input", errorOutput.isEmpty());
  }

  @Test
  public void testHandlesWhitespaceOnlyInput() {
    crashAnalyzer.analyzeCrashInformation("   \n\t\n   ");

    String errorOutput = errContent.toString();
    assertTrue("Should produce no output for whitespace-only input", errorOutput.isEmpty());
  }

  @Test
  public void testDetectsMultipleOOMInSameLogcat() {
    String logcatOutput =
        "E/AndroidRuntime(12345): java.lang.OutOfMemoryError: First OOM\n"
            + "I/SomeLog(99999): Some normal log\n"
            + "E/AndroidRuntime(67890): java.lang.OutOfMemoryError: Second OOM\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect multiple OOM errors", errorOutput.contains("Out of memory error detected"));
    assertTrue("Should include first OOM", errorOutput.contains("First OOM"));
  }

  @Test
  public void testOutputContainsEndMarker() {
    String logcatOutput =
        "E/AndroidRuntime(12345): java.lang.OutOfMemoryError: Test error\n"
            + "E/AndroidRuntime(12345): \tat com.facebook.example.Test.method(Test.java:1)\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue("Should include end marker", errorOutput.contains("=== END AIT CRASH ANALYSIS ==="));
  }

  @Test
  public void testOOMWithoutStackTrace() {
    String logcatOutput = "E/AndroidRuntime(12345): java.lang.OutOfMemoryError: Allocation failed";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue("Should detect OOM without stack trace", errorOutput.contains("Allocation failed"));
  }

  @Test
  public void testOOMInDifferentBuffer() {
    String logcatOutput =
        "=== CRASH BUFFER ===\n"
            + "E/AndroidRuntime(12345): java.lang.OutOfMemoryError: Buffer overflow\n"
            + "E/AndroidRuntime(12345): \tat com.example.Test(Test.java:100)\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue("Should detect OOM in different buffer", errorOutput.contains("Buffer overflow"));
    assertTrue("Should include AIT detection prefix", errorOutput.contains("AIT_CRASH_DETECTION"));
  }

  @Test
  public void testCaseInsensitiveDetection() {
    String logcatOutput = "E/AndroidRuntime(12345): java.lang.OutOfMemoryError: memory exhausted";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue("Should detect OOM regardless of case", errorOutput.contains("memory exhausted"));
  }

  @Test
  public void testDetectsOOMWithLongStackTrace() {
    String logcatOutput =
        "E/AndroidRuntime(12345): java.lang.OutOfMemoryError: Failed to allocate\n"
            + "E/AndroidRuntime(12345): \tat com.facebook.example.Class1.method1(Class1.java:10)\n"
            + "E/AndroidRuntime(12345): \tat com.facebook.example.Class2.method2(Class2.java:20)\n"
            + "E/AndroidRuntime(12345): \tat com.facebook.example.Class3.method3(Class3.java:30)\n"
            + "E/AndroidRuntime(12345): \tat com.facebook.example.Class4.method4(Class4.java:40)\n"
            + "E/AndroidRuntime(12345): \tat com.facebook.example.Class5.method5(Class5.java:50)\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect OOM with long stack trace", errorOutput.contains("Failed to allocate"));
    assertTrue(
        "Should extract at least one stack frame", errorOutput.contains("at com.facebook.example"));
  }

  @Test
  public void testIgnoresNonCrashExceptions() {
    String logcatOutput =
        "W/System.err(12345): java.io.IOException: File not found\n"
            + "W/System.err(12345): \tat"
            + " com.facebook.example.FileReader.read(FileReader.java:42)\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should always print analysis header", errorOutput.contains("=== AIT CRASH ANALYSIS ==="));
    assertTrue("Should print no crashes detected", errorOutput.contains("No crashes detected"));
    assertFalse(
        "Should not detect non-OOM exceptions as crashes",
        errorOutput.contains("Out of memory error detected"));
  }

  @Test
  public void testOOMAtEndOfLogcat() {
    String logcatOutput =
        "I/SomeLog(1234): Normal log entry\n"
            + "D/AnotherLog(5678): Debug message\n"
            + "E/AndroidRuntime(12345): java.lang.OutOfMemoryError: At end of log";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue("Should detect OOM at end of logcat", errorOutput.contains("At end of log"));
  }

  @Test
  public void testOOMAtStartOfLogcat() {
    String logcatOutput =
        "E/AndroidRuntime(12345): java.lang.OutOfMemoryError: At start of log\n"
            + "I/SomeLog(1234): Normal log entry\n"
            + "D/AnotherLog(5678): Debug message\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue("Should detect OOM at start of logcat", errorOutput.contains("At start of log"));
  }
}
