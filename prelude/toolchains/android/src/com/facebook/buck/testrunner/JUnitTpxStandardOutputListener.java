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
import java.io.ByteArrayOutputStream;
import java.util.HashMap;
import java.util.Map;
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
  }

  // Map to store per-test logging state using test description as key
  private final Map<String, TestLogState> testLogStates = new HashMap<>();

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
    return String.format("%s (%s)", description.getMethodName(), description.getClassName());
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
      // Clean up logging handlers
      JavaUtilLoggingHelper.cleanupLogging(testLogState.getLogHandlers());

      // Remove the test log state from the map
      testLogStates.remove(testName);
    }

    listener.testFinished(testName);
  }
}
