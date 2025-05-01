/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.testrunner;

import com.facebook.buck.testresultsoutput.TestResultsOutputSender;
import org.junit.runner.Description;
import org.junit.runner.notification.Failure;
import org.junit.runner.notification.RunListener;

/**
 * Extends org.junit.runner.notification.RunListener. Meant to be registered with RunNotifier to be
 * notified of events that occurred during a test run.
 */
public class JUnitTpxStandardOutputListener extends RunListener {
  private final TpxStandardOutputTestListener listener;

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
    listener.testStarted(getFullTestName(description));
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
    listener.testFailed(
        getFullTestName(failure.getDescription()), failure.getException().toString());
  }

  /**
   * Called when an atomic test flags that it assumes a condition that is false
   *
   * @param failure describes the test that failed and the {@link
   *     org.junit.AssumptionViolatedException} that was thrown
   */
  @Override
  public void testAssumptionFailure(Failure failure) {
    listener.testAssumptionFailure(
        getFullTestName(failure.getDescription()), failure.getException().toString());
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
    listener.testFinished(getFullTestName(description));
  }
}
