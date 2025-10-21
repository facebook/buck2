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

  @Test
  public void testDetectsStackOverflowError() {
    String logcatOutput =
        "E/AndroidRuntime(12345): FATAL EXCEPTION: main\n"
            + "E/AndroidRuntime(12345): java.lang.StackOverflowError\n"
            + "E/AndroidRuntime(12345): \tat"
            + " com.facebook.example.RecursiveClass.recursiveMethod(RecursiveClass.java:42)\n"
            + "E/AndroidRuntime(12345): \tat"
            + " com.facebook.example.RecursiveClass.recursiveMethod(RecursiveClass.java:42)\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect StackOverflowError", errorOutput.contains("Stack overflow error detected"));
    assertTrue(
        "Should print crash analysis header", errorOutput.contains("=== AIT CRASH ANALYSIS ==="));
    assertTrue(
        "Should include StackOverflowError", errorOutput.contains("java.lang.StackOverflowError"));
    assertTrue("Should include stack trace", errorOutput.contains("recursiveMethod"));
  }

  @Test
  public void testDetectsStackOverflowErrorWithMessage() {
    String logcatOutput =
        "E/AndroidRuntime(67890): java.lang.StackOverflowError: stack size 8MB\n"
            + "E/AndroidRuntime(67890): \tat java.lang.Thread.run(Thread.java:100)\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect StackOverflowError", errorOutput.contains("Stack overflow error detected"));
    assertTrue("Should include error message", errorOutput.contains("stack size 8MB"));
  }

  @Test
  public void testDetectsStackOverflowErrorWithoutStackTrace() {
    String logcatOutput = "E/AndroidRuntime(12345): java.lang.StackOverflowError";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect StackOverflowError without stack trace",
        errorOutput.contains("Stack overflow error detected"));
    assertTrue("Should include error line", errorOutput.contains("java.lang.StackOverflowError"));
  }

  @Test
  public void testDetectsBothOOMAndStackOverflow() {
    String logcatOutput =
        "E/AndroidRuntime(12345): java.lang.OutOfMemoryError: Failed allocation\n"
            + "I/SomeLog(99999): Some normal log\n"
            + "E/AndroidRuntime(67890): java.lang.StackOverflowError\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect OutOfMemoryError", errorOutput.contains("Out of memory error detected"));
    assertTrue(
        "Should detect StackOverflowError", errorOutput.contains("Stack overflow error detected"));
    assertTrue("Should include OOM", errorOutput.contains("Failed allocation"));
    assertTrue("Should include SOE", errorOutput.contains("java.lang.StackOverflowError"));
  }

  @Test
  public void testStackOverflowAtEndOfLogcat() {
    String logcatOutput =
        "I/SomeLog(1234): Normal log entry\n"
            + "D/AnotherLog(5678): Debug message\n"
            + "E/AndroidRuntime(12345): java.lang.StackOverflowError";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect StackOverflowError at end of logcat",
        errorOutput.contains("Stack overflow error detected"));
  }

  @Test
  public void testStackOverflowAtStartOfLogcat() {
    String logcatOutput =
        "E/AndroidRuntime(12345): java.lang.StackOverflowError\n"
            + "I/SomeLog(1234): Normal log entry\n"
            + "D/AnotherLog(5678): Debug message\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect StackOverflowError at start of logcat",
        errorOutput.contains("Stack overflow error detected"));
  }

  @Test
  public void testStackOverflowWithDeepStackTrace() {
    String logcatOutput =
        "E/AndroidRuntime(12345): java.lang.StackOverflowError\n"
            + "E/AndroidRuntime(12345): \tat com.facebook.example.A.method(A.java:10)\n"
            + "E/AndroidRuntime(12345): \tat com.facebook.example.B.method(B.java:20)\n"
            + "E/AndroidRuntime(12345): \tat com.facebook.example.C.method(C.java:30)\n"
            + "E/AndroidRuntime(12345): \tat com.facebook.example.A.method(A.java:10)\n"
            + "E/AndroidRuntime(12345): \tat com.facebook.example.B.method(B.java:20)\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect StackOverflowError with deep stack trace",
        errorOutput.contains("Stack overflow error detected"));
    assertTrue(
        "Should extract at least one stack frame", errorOutput.contains("at com.facebook.example"));
  }

  @Test
  public void testDetectsNullPointerException() {
    String logcatOutput =
        "E/AndroidRuntime(12345): FATAL EXCEPTION: main\n"
            + "E/AndroidRuntime(12345): java.lang.NullPointerException\n"
            + "E/AndroidRuntime(12345): \tat"
            + " com.facebook.example.MyClass.handleObject(MyClass.java:50)\n"
            + "E/AndroidRuntime(12345): \tat"
            + " com.facebook.example.MainActivity.onCreate(MainActivity.java:123)\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect NullPointerException",
        errorOutput.contains("Null pointer exception detected"));
    assertTrue(
        "Should print crash analysis header", errorOutput.contains("=== AIT CRASH ANALYSIS ==="));
    assertTrue(
        "Should include NullPointerException",
        errorOutput.contains("java.lang.NullPointerException"));
    assertTrue("Should include stack trace", errorOutput.contains("handleObject"));
  }

  @Test
  public void testDetectsNullPointerExceptionWithMessage() {
    String logcatOutput =
        "E/AndroidRuntime(67890): java.lang.NullPointerException: Attempt to invoke virtual"
            + " method 'java.lang.String.toString()' on a null object reference\n"
            + "E/AndroidRuntime(67890): \tat com.facebook.example.Utils.process(Utils.java:100)\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect NullPointerException",
        errorOutput.contains("Null pointer exception detected"));
    assertTrue(
        "Should include error message", errorOutput.contains("Attempt to invoke virtual method"));
  }

  @Test
  public void testDetectsNullPointerExceptionWithoutStackTrace() {
    String logcatOutput = "E/AndroidRuntime(12345): java.lang.NullPointerException";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect NullPointerException without stack trace",
        errorOutput.contains("Null pointer exception detected"));
    assertTrue("Should include error line", errorOutput.contains("java.lang.NullPointerException"));
  }

  @Test
  public void testDetectsAllThreeErrorTypes() {
    String logcatOutput =
        "E/AndroidRuntime(12345): java.lang.OutOfMemoryError: Failed allocation\n"
            + "I/SomeLog(99999): Some normal log\n"
            + "E/AndroidRuntime(67890): java.lang.StackOverflowError\n"
            + "I/AnotherLog(11111): Another log\n"
            + "E/AndroidRuntime(11122): java.lang.NullPointerException\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect OutOfMemoryError", errorOutput.contains("Out of memory error detected"));
    assertTrue(
        "Should detect StackOverflowError", errorOutput.contains("Stack overflow error detected"));
    assertTrue(
        "Should detect NullPointerException",
        errorOutput.contains("Null pointer exception detected"));
    assertTrue("Should include OOM", errorOutput.contains("Failed allocation"));
    assertTrue("Should include SOE", errorOutput.contains("java.lang.StackOverflowError"));
    assertTrue("Should include NPE", errorOutput.contains("java.lang.NullPointerException"));
  }

  @Test
  public void testNullPointerAtEndOfLogcat() {
    String logcatOutput =
        "I/SomeLog(1234): Normal log entry\n"
            + "D/AnotherLog(5678): Debug message\n"
            + "E/AndroidRuntime(12345): java.lang.NullPointerException";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect NullPointerException at end of logcat",
        errorOutput.contains("Null pointer exception detected"));
  }

  @Test
  public void testNullPointerAtStartOfLogcat() {
    String logcatOutput =
        "E/AndroidRuntime(12345): java.lang.NullPointerException\n"
            + "I/SomeLog(1234): Normal log entry\n"
            + "D/AnotherLog(5678): Debug message\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect NullPointerException at start of logcat",
        errorOutput.contains("Null pointer exception detected"));
  }

  @Test
  public void testNullPointerWithKotlinStackTrace() {
    String logcatOutput =
        "E/AndroidRuntime(12345): java.lang.NullPointerException\n"
            + "E/AndroidRuntime(12345): \tat"
            + " com.facebook.example.MyKotlinClass.doSomething(MyKotlinClass.kt:42)\n"
            + "E/AndroidRuntime(12345): \tat"
            + " com.facebook.example.MyKotlinClass$lambda$0(MyKotlinClass.kt:50)\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect NullPointerException with Kotlin stack trace",
        errorOutput.contains("Null pointer exception detected"));
    assertTrue("Should include Kotlin file", errorOutput.contains("MyKotlinClass.kt"));
  }

  @Test
  public void testMultipleNullPointerExceptions() {
    String logcatOutput =
        "E/AndroidRuntime(12345): java.lang.NullPointerException: First NPE\n"
            + "I/SomeLog(99999): Some normal log\n"
            + "E/AndroidRuntime(67890): java.lang.NullPointerException: Second NPE\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect multiple NullPointerExceptions",
        errorOutput.contains("Null pointer exception detected"));
    assertTrue("Should include first NPE", errorOutput.contains("First NPE"));
  }

  @Test
  public void testDetectsArrayIndexOutOfBoundsException() {
    String logcatOutput =
        "E/AndroidRuntime(12345): FATAL EXCEPTION: main\n"
            + "E/AndroidRuntime(12345): java.lang.ArrayIndexOutOfBoundsException: length=5;"
            + " index=10\n"
            + "E/AndroidRuntime(12345): \tat"
            + " com.facebook.example.ArrayProcessor.process(ArrayProcessor.java:50)\n"
            + "E/AndroidRuntime(12345): \tat"
            + " com.facebook.example.MainActivity.onCreate(MainActivity.java:123)\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect ArrayIndexOutOfBoundsException",
        errorOutput.contains("Index out of bounds exception detected"));
    assertTrue(
        "Should print crash analysis header", errorOutput.contains("=== AIT CRASH ANALYSIS ==="));
    assertTrue(
        "Should include ArrayIndexOutOfBoundsException",
        errorOutput.contains("java.lang.ArrayIndexOutOfBoundsException"));
    assertTrue("Should include error details", errorOutput.contains("length=5; index=10"));
  }

  @Test
  public void testDetectsIndexOutOfBoundsException() {
    String logcatOutput =
        "E/AndroidRuntime(67890): java.lang.IndexOutOfBoundsException: Index: 10, Size: 3\n"
            + "E/AndroidRuntime(67890): \tat java.util.ArrayList.get(ArrayList.java:437)\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect IndexOutOfBoundsException",
        errorOutput.contains("Index out of bounds exception detected"));
    assertTrue("Should include error message", errorOutput.contains("Index: 10, Size: 3"));
  }

  @Test
  public void testDetectsIndexOutOfBoundsExceptionWithoutStackTrace() {
    String logcatOutput =
        "E/AndroidRuntime(12345): java.lang.IndexOutOfBoundsException: Index out of range";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect IndexOutOfBoundsException without stack trace",
        errorOutput.contains("Index out of bounds exception detected"));
    assertTrue(
        "Should include error line", errorOutput.contains("java.lang.IndexOutOfBoundsException"));
  }

  @Test
  public void testDetectsAllFourErrorTypes() {
    String logcatOutput =
        "E/AndroidRuntime(12345): java.lang.OutOfMemoryError: Failed allocation\n"
            + "I/SomeLog(99999): Some normal log\n"
            + "E/AndroidRuntime(67890): java.lang.StackOverflowError\n"
            + "I/AnotherLog(11111): Another log\n"
            + "E/AndroidRuntime(11122): java.lang.NullPointerException\n"
            + "I/MoreLog(22222): More log\n"
            + "E/AndroidRuntime(33344): java.lang.ArrayIndexOutOfBoundsException: index=5\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect OutOfMemoryError", errorOutput.contains("Out of memory error detected"));
    assertTrue(
        "Should detect StackOverflowError", errorOutput.contains("Stack overflow error detected"));
    assertTrue(
        "Should detect NullPointerException",
        errorOutput.contains("Null pointer exception detected"));
    assertTrue(
        "Should detect ArrayIndexOutOfBoundsException",
        errorOutput.contains("Index out of bounds exception detected"));
    assertTrue("Should include OOM", errorOutput.contains("Failed allocation"));
    assertTrue("Should include SOE", errorOutput.contains("java.lang.StackOverflowError"));
    assertTrue("Should include NPE", errorOutput.contains("java.lang.NullPointerException"));
    assertTrue(
        "Should include AIOOBE", errorOutput.contains("java.lang.ArrayIndexOutOfBoundsException"));
  }

  @Test
  public void testIndexOutOfBoundsAtEndOfLogcat() {
    String logcatOutput =
        "I/SomeLog(1234): Normal log entry\n"
            + "D/AnotherLog(5678): Debug message\n"
            + "E/AndroidRuntime(12345): java.lang.IndexOutOfBoundsException: Index: 5, Size: 3";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect IndexOutOfBoundsException at end of logcat",
        errorOutput.contains("Index out of bounds exception detected"));
  }

  @Test
  public void testIndexOutOfBoundsAtStartOfLogcat() {
    String logcatOutput =
        "E/AndroidRuntime(12345): java.lang.ArrayIndexOutOfBoundsException: index=10\n"
            + "I/SomeLog(1234): Normal log entry\n"
            + "D/AnotherLog(5678): Debug message\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect ArrayIndexOutOfBoundsException at start of logcat",
        errorOutput.contains("Index out of bounds exception detected"));
  }

  @Test
  public void testIndexOutOfBoundsWithKotlinStackTrace() {
    String logcatOutput =
        "E/AndroidRuntime(12345): java.lang.IndexOutOfBoundsException\n"
            + "E/AndroidRuntime(12345): \tat"
            + " com.facebook.example.MyKotlinList.getItem(MyKotlinList.kt:42)\n"
            + "E/AndroidRuntime(12345): \tat"
            + " com.facebook.example.MyKotlinList$process$1.invoke(MyKotlinList.kt:50)\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect IndexOutOfBoundsException with Kotlin stack trace",
        errorOutput.contains("Index out of bounds exception detected"));
    assertTrue("Should include Kotlin file", errorOutput.contains("MyKotlinList.kt"));
  }

  @Test
  public void testMultipleIndexOutOfBoundsExceptions() {
    String logcatOutput =
        "E/AndroidRuntime(12345): java.lang.ArrayIndexOutOfBoundsException: First error\n"
            + "I/SomeLog(99999): Some normal log\n"
            + "E/AndroidRuntime(67890): java.lang.IndexOutOfBoundsException: Second error\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect multiple IndexOutOfBoundsExceptions",
        errorOutput.contains("Index out of bounds exception detected"));
    assertTrue("Should include first error", errorOutput.contains("First error"));
  }

  @Test
  public void testArrayIndexOutOfBoundsWithNegativeIndex() {
    String logcatOutput =
        "E/AndroidRuntime(12345): java.lang.ArrayIndexOutOfBoundsException: length=5; index=-1\n"
            + "E/AndroidRuntime(12345): \tat"
            + " com.facebook.example.ArrayHandler.get(ArrayHandler.java:10)\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect negative index error",
        errorOutput.contains("Index out of bounds exception detected"));
    assertTrue("Should include negative index", errorOutput.contains("index=-1"));
  }

  @Test
  public void testDetectsIllegalStateException() {
    String logcatOutput =
        "E/AndroidRuntime(12345): FATAL EXCEPTION: main\n"
            + "E/AndroidRuntime(12345): java.lang.IllegalStateException: Test illegal state crash\n"
            + "E/AndroidRuntime(12345): \tat"
            + " com.facebook.example.StateManager.doAction(StateManager.java:50)\n"
            + "E/AndroidRuntime(12345): \tat"
            + " com.facebook.example.MainActivity.onCreate(MainActivity.java:123)\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect IllegalStateException",
        errorOutput.contains("Illegal state exception detected"));
    assertTrue(
        "Should print crash analysis header", errorOutput.contains("=== AIT CRASH ANALYSIS ==="));
    assertTrue(
        "Should include IllegalStateException",
        errorOutput.contains("java.lang.IllegalStateException"));
    assertTrue("Should include error message", errorOutput.contains("Test illegal state crash"));
  }

  @Test
  public void testDetectsIllegalStateExceptionWithDetailedMessage() {
    String logcatOutput =
        "E/AndroidRuntime(67890): java.lang.IllegalStateException: Fragment not attached to"
            + " Activity\n"
            + "E/AndroidRuntime(67890): \tat"
            + " androidx.fragment.app.Fragment.getContext(Fragment.java:100)\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect IllegalStateException",
        errorOutput.contains("Illegal state exception detected"));
    assertTrue(
        "Should include error message", errorOutput.contains("Fragment not attached to Activity"));
  }

  @Test
  public void testDetectsIllegalStateExceptionWithoutStackTrace() {
    String logcatOutput = "E/AndroidRuntime(12345): java.lang.IllegalStateException: Invalid state";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect IllegalStateException without stack trace",
        errorOutput.contains("Illegal state exception detected"));
    assertTrue(
        "Should include error line", errorOutput.contains("java.lang.IllegalStateException"));
  }

  @Test
  public void testDetectsAllFiveErrorTypes() {
    String logcatOutput =
        "E/AndroidRuntime(12345): java.lang.OutOfMemoryError: Failed allocation\n"
            + "I/SomeLog(99999): Some normal log\n"
            + "E/AndroidRuntime(67890): java.lang.StackOverflowError\n"
            + "I/AnotherLog(11111): Another log\n"
            + "E/AndroidRuntime(11122): java.lang.NullPointerException\n"
            + "I/MoreLog(22222): More log\n"
            + "E/AndroidRuntime(33344): java.lang.ArrayIndexOutOfBoundsException: index=5\n"
            + "I/EvenMoreLog(44444): Even more log\n"
            + "E/AndroidRuntime(55566): java.lang.IllegalStateException: Invalid state\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect OutOfMemoryError", errorOutput.contains("Out of memory error detected"));
    assertTrue(
        "Should detect StackOverflowError", errorOutput.contains("Stack overflow error detected"));
    assertTrue(
        "Should detect NullPointerException",
        errorOutput.contains("Null pointer exception detected"));
    assertTrue(
        "Should detect ArrayIndexOutOfBoundsException",
        errorOutput.contains("Index out of bounds exception detected"));
    assertTrue(
        "Should detect IllegalStateException",
        errorOutput.contains("Illegal state exception detected"));
  }

  @Test
  public void testIllegalStateAtEndOfLogcat() {
    String logcatOutput =
        "I/SomeLog(1234): Normal log entry\n"
            + "D/AnotherLog(5678): Debug message\n"
            + "E/AndroidRuntime(12345): java.lang.IllegalStateException: At end";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect IllegalStateException at end of logcat",
        errorOutput.contains("Illegal state exception detected"));
  }

  @Test
  public void testIllegalStateAtStartOfLogcat() {
    String logcatOutput =
        "E/AndroidRuntime(12345): java.lang.IllegalStateException: At start\n"
            + "I/SomeLog(1234): Normal log entry\n"
            + "D/AnotherLog(5678): Debug message\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect IllegalStateException at start of logcat",
        errorOutput.contains("Illegal state exception detected"));
  }

  @Test
  public void testIllegalStateWithKotlinStackTrace() {
    String logcatOutput =
        "E/AndroidRuntime(12345): java.lang.IllegalStateException: Coroutine state error\n"
            + "E/AndroidRuntime(12345): \tat"
            + " com.facebook.example.MyCoroutine.execute(MyCoroutine.kt:42)\n"
            + "E/AndroidRuntime(12345): \tat"
            + " kotlinx.coroutines.CoroutineScope.launch(CoroutineScope.kt:50)\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect IllegalStateException with Kotlin stack trace",
        errorOutput.contains("Illegal state exception detected"));
    assertTrue("Should include Kotlin file", errorOutput.contains("MyCoroutine.kt"));
  }

  @Test
  public void testMultipleIllegalStateExceptions() {
    String logcatOutput =
        "E/AndroidRuntime(12345): java.lang.IllegalStateException: First error\n"
            + "I/SomeLog(99999): Some normal log\n"
            + "E/AndroidRuntime(67890): java.lang.IllegalStateException: Second error\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect multiple IllegalStateExceptions",
        errorOutput.contains("Illegal state exception detected"));
    assertTrue("Should include first error", errorOutput.contains("First error"));
  }

  @Test
  public void testIllegalStateWithFragmentError() {
    String logcatOutput =
        "E/AndroidRuntime(12345): java.lang.IllegalStateException: Can not perform this action"
            + " after onSaveInstanceState\n"
            + "E/AndroidRuntime(12345): \tat"
            + " androidx.fragment.app.FragmentManager.checkStateLoss(FragmentManager.java:1500)\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect fragment IllegalStateException",
        errorOutput.contains("Illegal state exception detected"));
    assertTrue(
        "Should include fragment error message", errorOutput.contains("after onSaveInstanceState"));
  }

  @Test
  public void testIllegalStateWithViewError() {
    String logcatOutput =
        "E/AndroidRuntime(12345): java.lang.IllegalStateException: The specified child already"
            + " has a parent\n"
            + "E/AndroidRuntime(12345): \tat"
            + " android.view.ViewGroup.addViewInner(ViewGroup.java:4950)\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect view IllegalStateException",
        errorOutput.contains("Illegal state exception detected"));
    assertTrue("Should include view error message", errorOutput.contains("already has a parent"));
  }

  @Test
  public void testDetectsSIGABRTSignal() {
    String logcatOutput =
        "F/libc(12345): Fatal signal 6 (SIGABRT), code -6 in tid 12345 (com.facebook.app)\n"
            + "I/DEBUG(1234): *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** ***\n"
            + "I/DEBUG(1234): Build fingerprint: 'google/sdk_gphone64_x86_64/generic_x86_64:11'\n"
            + "I/DEBUG(1234): backtrace:\n"
            + "I/DEBUG(1234):     #00 pc 00000000000a1234  /system/lib64/libc.so (abort+123)\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue("Should detect SIGABRT signal", errorOutput.contains("SIGABRT signal detected"));
    assertTrue(
        "Should print crash analysis header", errorOutput.contains("=== AIT CRASH ANALYSIS ==="));
    assertTrue("Should include signal line", errorOutput.contains("Fatal signal 6"));
    assertTrue("Should include backtrace", errorOutput.contains("backtrace:"));
  }

  @Test
  public void testDetectsSIGABRTWithUppercase() {
    String logcatOutput =
        "F/libc(12345): Fatal signal 6 (SIGABRT), code -6 in tid 12345\n"
            + "I/DEBUG(1234): backtrace:\n"
            + "I/DEBUG(1234):     #00 pc 00001234  /system/lib64/libc.so\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect SIGABRT with uppercase", errorOutput.contains("SIGABRT signal detected"));
    assertTrue("Should include signal info", errorOutput.contains("SIGABRT"));
  }

  @Test
  public void testDetectsSIGABRTWithLowercase() {
    String logcatOutput =
        "F/libc(12345): fatal signal 6 (sigabrt), code -6 in tid 12345\n"
            + "I/DEBUG(1234): backtrace:\n"
            + "I/DEBUG(1234):     #00 pc 00001234  /system/lib64/libc.so\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect SIGABRT case-insensitively",
        errorOutput.contains("SIGABRT signal detected"));
  }

  @Test
  public void testDetectsSignal6() {
    String logcatOutput =
        "F/libc(12345): signal 6 received\n"
            + "I/DEBUG(1234): backtrace:\n"
            + "I/DEBUG(1234):     #00 pc 00001234  /system/lib64/libc.so\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue("Should detect signal 6", errorOutput.contains("SIGABRT signal detected"));
    assertTrue("Should include signal info", errorOutput.contains("signal 6"));
  }

  @Test
  public void testDetectsAbortKeyword() {
    String logcatOutput =
        "F/libc(12345): Abort message: 'some error message'\n"
            + "I/DEBUG(1234): backtrace:\n"
            + "I/DEBUG(1234):     #00 pc 00001234  /system/lib64/libc.so\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue("Should detect Abort keyword", errorOutput.contains("SIGABRT signal detected"));
    assertTrue("Should include abort message", errorOutput.contains("Abort message"));
  }

  @Test
  public void testSIGABRTWithMultipleBacktraceLines() {
    String logcatOutput =
        "F/libc(12345): Fatal signal 6 (SIGABRT), code -6 in tid 12345\n"
            + "I/DEBUG(1234): backtrace:\n"
            + "I/DEBUG(1234):     #00 pc 00001234  /system/lib64/libc.so (abort+123)\n"
            + "I/DEBUG(1234):     #01 pc 00005678  /system/lib64/libc.so (raise+45)\n"
            + "I/DEBUG(1234):     #02 pc 00009abc "
            + " /data/app/com.facebook.app/lib/x86_64/libnative.so\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect SIGABRT with multiple backtrace lines",
        errorOutput.contains("SIGABRT signal detected"));
    assertTrue("Should include first backtrace line", errorOutput.contains("#00 pc"));
  }

  @Test
  public void testSIGABRTWithNativeLibrary() {
    String logcatOutput =
        "F/libc(12345): Fatal signal 6 (SIGABRT), code -6 in tid 12345\n"
            + "I/DEBUG(1234): backtrace:\n"
            + "I/DEBUG(1234):     #00 pc 00001234"
            + "  /data/app/com.facebook.app/lib/arm64/libnative.so\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect SIGABRT with native library",
        errorOutput.contains("SIGABRT signal detected"));
    assertTrue("Should include native library", errorOutput.contains("libnative.so"));
  }

  @Test
  public void testSIGABRTAtEndOfLogcat() {
    String logcatOutput =
        "I/SomeLog(1234): Normal log entry\n"
            + "D/AnotherLog(5678): Debug message\n"
            + "F/libc(12345): Fatal signal 6 (SIGABRT), code -6 in tid 12345";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect SIGABRT at end of logcat", errorOutput.contains("SIGABRT signal detected"));
  }

  @Test
  public void testSIGABRTAtStartOfLogcat() {
    String logcatOutput =
        "F/libc(12345): Fatal signal 6 (SIGABRT), code -6 in tid 12345\n"
            + "I/SomeLog(1234): Normal log entry\n"
            + "D/AnotherLog(5678): Debug message\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect SIGABRT at start of logcat",
        errorOutput.contains("SIGABRT signal detected"));
  }

  @Test
  public void testSIGABRTWithDebugInfo() {
    String logcatOutput =
        "F/libc(12345): Fatal signal 6 (SIGABRT), code -6 in tid 12345 (Thread-5)\n"
            + "I/DEBUG(1234): *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** ***\n"
            + "I/DEBUG(1234): Build fingerprint: 'google/sdk_gphone64_x86_64/generic_x86_64:11'\n"
            + "I/DEBUG(1234): Revision: '0'\n"
            + "I/DEBUG(1234): ABI: 'x86_64'\n"
            + "I/DEBUG(1234): backtrace:\n"
            + "I/DEBUG(1234):     #00 pc 00001234  /system/lib64/libc.so\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect SIGABRT with debug info", errorOutput.contains("SIGABRT signal detected"));
    assertTrue("Should include thread name", errorOutput.contains("Thread-5"));
  }

  @Test
  public void testMultipleSIGABRTOccurrences() {
    String logcatOutput =
        "F/libc(12345): Fatal signal 6 (SIGABRT), code -6 in tid 12345\n"
            + "I/SomeLog(99999): Some normal log\n"
            + "F/libc(67890): Fatal signal 6 (SIGABRT), code -6 in tid 67890\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect multiple SIGABRT occurrences",
        errorOutput.contains("SIGABRT signal detected"));
  }

  @Test
  public void testSIGABRTWithExceptionAndSignal() {
    String logcatOutput =
        "E/AndroidRuntime(12345): java.lang.NullPointerException\n"
            + "I/SomeLog(99999): Some normal log\n"
            + "F/libc(67890): Fatal signal 6 (SIGABRT), code -6 in tid 67890\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect NullPointerException",
        errorOutput.contains("Null pointer exception detected"));
    assertTrue("Should detect SIGABRT signal", errorOutput.contains("SIGABRT signal detected"));
  }

  @Test
  public void testSIGABRTWithPcAndSymbol() {
    String logcatOutput =
        "F/libc(12345): Fatal signal 6 (SIGABRT), code -6\n"
            + "I/DEBUG(1234): backtrace:\n"
            + "I/DEBUG(1234):     #00 pc 00012345  /system/lib64/libc.so (abort+256)\n"
            + "I/DEBUG(1234):     #01 pc 00067890  /system/lib64/libc.so (__assert_fail+128)\n";

    crashAnalyzer.analyzeCrashInformation(logcatOutput);

    String errorOutput = errContent.toString();
    assertTrue(
        "Should detect SIGABRT with symbols", errorOutput.contains("SIGABRT signal detected"));
    assertTrue("Should include pc info", errorOutput.contains("pc 00012345"));
  }
}
