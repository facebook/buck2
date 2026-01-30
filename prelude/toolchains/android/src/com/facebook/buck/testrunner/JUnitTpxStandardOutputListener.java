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

import com.facebook.buck.testresultsoutput.TestResultsOutputSender;
import com.facebook.buck.testrunner.JavaUtilLoggingHelper.LogHandlers;
import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonGenerator;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Level;
import org.junit.runner.Description;
import org.junit.runner.notification.Failure;
import org.junit.runner.notification.RunListener;

/**
 * Extends org.junit.runner.notification.RunListener. Meant to be registered with RunNotifier to be
 * notified of events that occurred during a test run.
 */
public class JUnitTpxStandardOutputListener extends RunListener {
  private final TpxStandardOutputTestListener listener;
  private final Level stdOutLogLevel = Level.INFO;
  private final Level stdErrLogLevel = Level.WARNING;

  /**
   * Class to hold logging state for each test. This allows tests to run in parallel without sharing
   * logging state.
   */
  private static class TestLogState {
    private ByteArrayOutputStream julLogBytes;
    private ByteArrayOutputStream julErrLogBytes;
    private LogHandlers logHandlers;
    private StandardOutputRecorder stdoutRecorder;
    private StandardOutputRecorder stderrRecorder;

    public TestLogState() {
      this.julLogBytes = new ByteArrayOutputStream();
      this.julErrLogBytes = new ByteArrayOutputStream();
    }

    public ByteArrayOutputStream getJulLogBytes() {
      return julLogBytes;
    }

    public ByteArrayOutputStream getJulErrLogBytes() {
      return julErrLogBytes;
    }

    public void setLogHandlers(LogHandlers logHandlers) {
      this.logHandlers = logHandlers;
    }

    public LogHandlers getLogHandlers() {
      return logHandlers;
    }

    public void setStdoutRecorder(StandardOutputRecorder stdoutRecorder) {
      this.stdoutRecorder = stdoutRecorder;
    }

    public StandardOutputRecorder getStdoutRecorder() {
      return stdoutRecorder;
    }

    public void setStderrRecorder(StandardOutputRecorder stderrRecorder) {
      this.stderrRecorder = stderrRecorder;
    }

    public StandardOutputRecorder getStderrRecorder() {
      return stderrRecorder;
    }
  }

  // Map to store per-test logging state using test description as key.
  // Uses ConcurrentHashMap to support parallel test execution.
  private final Map<String, TestLogState> testLogStates = new ConcurrentHashMap<>();

  public JUnitTpxStandardOutputListener(TestResultsOutputSender sender) {
    this.listener = new TpxStandardOutputTestListener(sender);
  }

  /**
   * Gets the full test name for a org.junit.runner.Description
   *
   * @param test the Description to get the name for
   * @return the full test name
   */
  private static String getFullTestName(Description description) {
    return getFullTestName(description.getMethodName(), description.getClassName());
  }

  /**
   * Gets the full test name from method and class names.
   *
   * @param methodName the test method name
   * @param className the test class name
   * @return the full test name in format "methodName (className)"
   */
  static String getFullTestName(String methodName, String className) {
    return String.format("%s (%s)", methodName, className);
  }

  /**
   * Called when an atomic test is about to be started.
   *
   * @param description the description of the test that is about to be run (generally a class and
   *     method name)
   */
  @Override
  public void testStarted(Description description) {
    String testName = getFullTestName(description);

    // Create new test log state for this test
    TestLogState testLogState = new TestLogState();

    // Set up logging handlers
    LogHandlers logHandlers =
        JavaUtilLoggingHelper.setupLogging(
            testLogState.getJulLogBytes(),
            testLogState.getJulErrLogBytes(),
            this.stdOutLogLevel,
            this.stdErrLogLevel);

    testLogState.setLogHandlers(logHandlers);

    // Start capturing stdout/stderr for per-test artifacts
    try {
      testLogState.setStdoutRecorder(StandardOutputRecorder.stdOut(this.stdOutLogLevel).record());
      testLogState.setStderrRecorder(StandardOutputRecorder.stdErr(this.stdErrLogLevel).record());
    } catch (IOException e) {
      // Log warning but continue - artifact capture is best-effort
      System.err.println("Warning: Failed to set up stdout/stderr capture: " + e.getMessage());
    }

    // Store the test log state in the map
    testLogStates.put(testName, testLogState);

    listener.testStarted(testName);
  }

  /**
   * Called when an atomic test fails, or when a listener throws an exception.
   *
   * <p>In the case of a failure of an atomic test, this method will be called with the same {@code
   * Description} passed to {@link #testStarted(Description)}, from the same thread that called
   * {@link #testStarted(Description)}.
   *
   * <p>In the case of a listener throwing an exception, this will be called with a {@code
   * Description} of {@link Description#TEST_MECHANISM}, and may be called on an arbitrary thread.
   *
   * @param failure describes the test that failed and the exception that was thrown
   */
  @Override
  public void testFailure(Failure failure) {
    listener.testFailed(getFullTestName(failure.getDescription()), failure.getTrace());
  }

  /**
   * Called when an atomic test flags that it assumes a condition that is false
   *
   * @param failure describes the test that failed and the {@link
   *     org.junit.AssumptionViolatedException} that was thrown
   */
  @Override
  public void testAssumptionFailure(Failure failure) {
    listener.testAssumptionFailure(getFullTestName(failure.getDescription()), failure.getTrace());
  }

  /**
   * Called when a test will not be run, generally because a test method is annotated with {@link
   * org.junit.Ignore}.
   *
   * @param description describes the test that will not be run
   */
  @Override
  public void testIgnored(Description description) {
    listener.testIgnored(getFullTestName(description));
  }

  /**
   * Called when an atomic test has finished, whether the test succeeds or fails.
   *
   * @param description the description of the test that just ran
   */
  @Override
  public void testFinished(Description description) {
    String testName = getFullTestName(description);

    // Get the test log state from the map
    TestLogState testLogState = testLogStates.get(testName);

    if (testLogState != null) {
      // Complete stdout/stderr capture
      if (testLogState.getStdoutRecorder() != null) {
        testLogState.getStdoutRecorder().complete();
      }
      if (testLogState.getStderrRecorder() != null) {
        testLogState.getStderrRecorder().complete();
      }

      // Write per-test artifacts if directories are available
      String artifactsDir = System.getenv("TEST_RESULT_ARTIFACTS_DIR");
      String annotationsDir = System.getenv("TEST_RESULT_ARTIFACT_ANNOTATIONS_DIR");

      if (artifactsDir != null && annotationsDir != null) {
        // Use the same test name format as TPX: "methodName (className)"
        String testCaseName = getFullTestName(description);
        writePerTestArtifacts(testLogState, artifactsDir, annotationsDir, testCaseName);
      }

      // Clean up logging handlers
      JavaUtilLoggingHelper.cleanupLogging(testLogState.getLogHandlers());

      // Remove the test log state from the map
      testLogStates.remove(testName);
    }

    // Notify listener that test finished (stdout/stderr only go to artifact files, not Test Output)
    listener.testFinished(testName);
  }

  /**
   * Writes per-test stdout/stderr artifacts and their annotation files.
   *
   * @param state the test log state containing captured output
   * @param artifactsDir directory where artifact files should be written
   * @param annotationsDir directory where annotation files should be written
   * @param testCaseName the test case name in format "methodName (className)"
   */
  private void writePerTestArtifacts(
      TestLogState state, String artifactsDir, String annotationsDir, String testCaseName) {
    // Get captured output from the recorders. Each recorder captures two streams:
    // 1. Direct prints (System.out/System.err) - e.g., System.out.println() in test code
    // 2. java.util.logging (JUL) messages - structured logs from libraries/frameworks
    // The header ("====DEBUG LOGS====" or "====ERROR LOGS====") separates these two streams
    // in the artifact file, helping developers distinguish test output from framework logs.
    String stdout = null;
    String stderr = null;

    if (state.getStdoutRecorder() != null) {
      stdout = state.getStdoutRecorder().toString(true, "====DEBUG LOGS====\n");
    }
    if (state.getStderrRecorder() != null) {
      stderr = state.getStderrRecorder().toString(true, "====ERROR LOGS====\n");
    }

    // Sanitize test name for use in filenames to avoid collisions between tests.
    // JUnit artifacts are written to a shared directory and TPX collects them at the end,
    // so we need unique filenames to preserve each test's output.
    // The test_case annotation associates each artifact with its specific test.
    String safeTestName =
        testCaseName.replace(" ", "_").replace("(", "").replace(")", "").replace("/", "_");

    // Write stdout artifact
    if (stdout != null) {
      String filename = "Test Stdout " + safeTestName;
      writeArtifactWithAnnotation(artifactsDir, annotationsDir, filename, stdout, testCaseName);
    }

    // Write stderr artifact
    if (stderr != null) {
      String filename = "Test Stderr " + safeTestName;
      writeArtifactWithAnnotation(artifactsDir, annotationsDir, filename, stderr, testCaseName);
    }
  }

  /**
   * Writes an artifact file and its corresponding annotation file.
   *
   * @param artifactsDir directory where the artifact file should be written
   * @param annotationsDir directory where the annotation file should be written
   * @param filename name of the artifact file (annotation will have .annotation appended)
   * @param content content to write to the artifact file
   * @param testCaseName the test case name in format "methodName (className)" for annotation
   */
  private void writeArtifactWithAnnotation(
      String artifactsDir,
      String annotationsDir,
      String filename,
      String content,
      String testCaseName) {
    try {
      // Write artifact file
      Path artifactPath = Paths.get(artifactsDir, filename);
      Files.write(artifactPath, content.getBytes(StandardCharsets.UTF_8));

      // Write annotation file with test_case field using Jackson core
      StringWriter stringWriter = new StringWriter();
      try (JsonGenerator gen = new JsonFactory().createGenerator(stringWriter)) {
        gen.writeStartObject();
        gen.writeObjectFieldStart("type");
        gen.writeObjectFieldStart("generic_text_log");
        gen.writeEndObject();
        gen.writeEndObject();
        gen.writeStringField("test_case", testCaseName);
        gen.writeEndObject();
      }
      String annotationJson = stringWriter.toString();
      Path annotationPath = Paths.get(annotationsDir, filename + ".annotation");
      Files.write(annotationPath, annotationJson.getBytes(StandardCharsets.UTF_8));
    } catch (IOException e) {
      // Log warning but don't fail the test - artifact writing is best-effort
      System.err.println(
          "Warning: Failed to write test artifact '" + filename + "': " + e.getMessage());
    }
  }

  /**
   * Reports an omitted test (e.g., @Ignore) to TPX with OMIT status so it won't be retried.
   *
   * @param testName the full test name in format "methodName (className)"
   * @param reason the reason the test was omitted
   */
  public void reportOmittedTest(String testName, String reason) {
    listener.testOmitted(testName, reason);
  }
}
