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

import com.android.ddmlib.IDevice;
import com.android.ddmlib.testrunner.ITestRunListener;
import com.android.ddmlib.testrunner.TestIdentifier;
import com.facebook.buck.testresultsoutput.TestResultsOutputSender;
import java.util.Map;

/**
 * InstrumentationTpxStandardOutputTestListener is a test event listener for Android Instrumentation
 * Test for the TPX Standard Output format.
 *
 * <p>It implements the ITestRunListener interface, which is best documented here:
 * https://developer.android.com/reference/androidx/test/orchestrator/listeners/result/ITestRunListener
 *
 * <p>The sequence of calls copied from the above link, where [ ] indicates optional calls:
 *
 * <p>testRunStarted testStarted [testFailed] [testAssumptionFailure] [testIgnored] testEnded ....
 * [testRunFailed] testRunEnded
 */
public class InstrumentationTpxStandardOutputTestListener implements ITestRunListener {
  private final TpxStandardOutputTestListener listener;
  private final IDevice mDevice;

  public InstrumentationTpxStandardOutputTestListener(
      TestResultsOutputSender sender, IDevice device) {
    this.listener = new TpxStandardOutputTestListener(sender);
    this.mDevice = device;
  }

  /**
   * Gets the full test name for a test identifier.
   *
   * @param test the test identifier to get the name for
   * @return the full test name
   */
  private static String getFullTestName(TestIdentifier test) {
    return String.format("%s (%s)", test.getTestName(), test.getClassName());
  }

  /**
   * Reports the start of an individual test case.
   *
   * @param test identifies the test
   */
  @Override
  public void testStarted(TestIdentifier test) {
    listener.testStarted(getFullTestName(test));
  }

  /**
   * Reports the failure of a individual test case.
   *
   * <p>Will be called between testStarted and testEnded.
   *
   * @param test identifies the test
   * @param trace stack trace of failure
   */
  @Override
  public void testFailed(TestIdentifier test, String trace) {
    if (mDevice != null && CrashCapturer.deviceHasCrashLogs(trace)) {
      trace = CrashCapturer.addDeviceLogcatTrace(mDevice, trace);
    }

    listener.testFailed(getFullTestName(test), trace);
  }

  /**
   * Called when an atomic test flags that it assumes a condition that is false
   *
   * @param test identifies the test
   * @param trace stack trace of failure
   */
  @Override
  public void testAssumptionFailure(TestIdentifier test, String trace) {
    listener.testAssumptionFailure(getFullTestName(test), trace);
  }

  /**
   * Called when a test will not be run, generally because a test method is annotated with
   * org.junit.Ignore.
   *
   * @param test identifies the test
   */
  @Override
  public void testIgnored(TestIdentifier test) {
    listener.testIgnored(getFullTestName(test));
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
  @Override
  public void testEnded(TestIdentifier test, Map<String, String> testMetrics) {
    listener.testFinished(getFullTestName(test));
  }

  /**
   * Reports test run failed to complete due to a fatal error.
   *
   * @param errorMessage {@link String} describing reason for run failure.
   */
  @Override
  public void testRunFailed(String errorMessage) {}

  /**
   * Reports the start of a test run.
   *
   * @param runName the test run name
   * @param testCount total number of tests in test run
   */
  @Override
  public void testRunStarted(String runName, int testCount) {}

  /**
   * Reports end of test run.
   *
   * @param elapsedTime device reported elapsed time, in milliseconds
   * @param runMetrics key-value pairs reported at the end of a test run
   */
  @Override
  public void testRunEnded(long elapsedTime, Map<String, String> runMetrics) {}

  /**
   * Reports test run stopped before completion due to a user request.
   *
   * <p>Deprecated and not currently used.
   *
   * @param elapsedTime device reported elapsed time, in milliseconds
   */
  @Override
  public void testRunStopped(long elapsedTime) {}
}
