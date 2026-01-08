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

import com.facebook.buck.testresultsoutput.TestResultsOutputEvent.TestStatus;
import com.facebook.buck.testresultsoutput.TestResultsOutputSender;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

/**
 * TpxStandardOutputTestListener sends test results to the TPX standard output format. Meant to be
 * shared between different Java test runners that may need to implement specific interfaces.
 */
public class TpxStandardOutputTestListener {
  private final TestResultsOutputSender sender;

  private Map<String, TestIdentifierStatus> testIdentifierStatuses = new HashMap<>();

  private class TestIdentifierStatus {
    private long startTime;
    private String trace;
    private TestStatus status;

    public TestIdentifierStatus(long startTime) {
      this.startTime = startTime;
    }

    public void setFailed(String trace) {
      this.status = TestStatus.FAIL;
      this.trace = trace;
    }

    public void setSkipped(String trace) {
      this.status = TestStatus.SKIP;
      this.trace = trace;
    }
  }

  public TpxStandardOutputTestListener(TestResultsOutputSender sender) {
    this.sender = sender;
  }

  /**
   * Registers a test identifier with the listener.
   *
   * @param identifier the test identifier to register
   * @param startedTime the time the test started, in milliseconds since Unix epoch
   */
  private void registerTest(String identifier, long startedTime) {
    testIdentifierStatuses.put(identifier, new TestIdentifierStatus(startedTime));
  }

  /**
   * Sends a test start event to the TestResultsOutputSender.
   *
   * @param identifier the test identifier to send the start event for
   * @param startedTime the time the test started, in milliseconds since Unix epoch
   */
  private void sendTestStart(String identifier, long startedTime) {
    sender.sendTestStart(identifier, startedTime);
  }

  /**
   * Reports the start of an individual test case.
   *
   * @param test identifies the test
   */
  public void testStarted(String identifier) {
    long startedTime = System.currentTimeMillis();
    registerTest(identifier, startedTime);
    sendTestStart(identifier, startedTime);
  }

  /**
   * Reports the failure of a individual test case.
   *
   * <p>Will be called between testStarted and testEnded.
   *
   * @param test identifies the test
   * @param trace stack trace of failure
   */
  public void testFailed(String identifier, String trace) {
    TestIdentifierStatus status = testIdentifierStatuses.get(identifier);
    if (status == null) {
      throw new IllegalStateException("testFailed called without testStarted");
    }

    status.setFailed(trace);
  }

  /**
   * Called when an atomic test flags that it assumes a condition that is false
   *
   * @param test identifies the test
   * @param trace stack trace of failure
   */
  public void testAssumptionFailure(String identifier, String trace) {
    TestIdentifierStatus status = testIdentifierStatuses.get(identifier);
    if (status == null) {
      throw new IllegalStateException("testAssumptionFailure called without testStarted");
    }

    status.setSkipped(trace);
  }

  /**
   * Called when a test will not be run, generally because a test method is annotated with
   * org.junit.Ignore.
   *
   * @param test identifies the test
   */
  public void testIgnored(String identifier) {
    TestIdentifierStatus status = testIdentifierStatuses.get(identifier);
    if (status == null) {
      throw new IllegalStateException("testIgnored called without testStarted");
    }

    status.setSkipped(
        "Test ignored, generally because the test method is annotated with org.junit.Ignore");
  }

  /**
   * Reports that a test was omitted (e.g., @Ignore annotation). Unlike testIgnored(), this method
   * handles tests that were filtered out before reaching the listener and creates a complete
   * start/finish sequence with OMIT status so TPX knows not to retry them.
   *
   * @param identifier the test identifier
   * @param reason the reason the test was omitted
   */
  public void testOmitted(String identifier, String reason) {
    long currentTime = System.currentTimeMillis();
    sender.sendTestStart(identifier, currentTime);
    sender.sendTestFinish(identifier, TestStatus.OMIT, currentTime, 0, Optional.of(reason));
  }

  /**
   * Reports the execution end of an individual test case.
   *
   * <p>If {@link #testFailed} was not invoked, this test passed. Also returns any key/value metrics
   * which may have been emitted during the test case's execution.
   *
   * @param test identifies the test
   * @param testMetrics a {@link Map} of the metrics emitted
   */
  public void testFinished(String identifier) {
    long endedTime = System.currentTimeMillis();
    TestIdentifierStatus testIdentifierStatus = testIdentifierStatuses.get(identifier);
    if (testIdentifierStatus == null) {
      throw new IllegalStateException("testEnded called without testStarted");
    }

    long duration = endedTime - testIdentifierStatus.startTime;
    TestStatus resultStatus = TestStatus.PASS;
    if (testIdentifierStatus.status != null) {
      resultStatus = testIdentifierStatus.status;
    }

    sender.sendTestFinish(
        identifier,
        resultStatus,
        endedTime,
        duration,
        Optional.ofNullable(testIdentifierStatus.trace));
  }

  /**
   * Reports test run failed to complete due to a fatal error.
   *
   * @param errorMessage {@link String} describing reason for run failure.
   */
  public void testRunFailed(String errorMessage) {}
}
