/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.testresultsoutput;

import com.facebook.buck.testresultsoutput.TestResultsOutputEvent.FinishEvent;
import com.facebook.buck.testresultsoutput.TestResultsOutputEvent.StartEvent;
import com.facebook.buck.testresultsoutput.TestResultsOutputEvent.TestStatus;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Optional;

/**
 * TestResultsOutputSender provides an interface for serializing test_result_output_spec.thrift Test
 * Events.
 */
public class TestResultsOutputSender implements AutoCloseable {
  private static final String TEST_RESULTS_OUTPUT_FILE_ENV_NAME = "TEST_RESULTS_OUTPUT_FILE";
  private final FileOutputStream fileOutputStream;

  /**
   * Constructor for TestResultsOutputSender.
   *
   * @param fileOutputStream The FileOutputStream to write to.
   */
  public TestResultsOutputSender(FileOutputStream fileOutputStream) {
    this.fileOutputStream = fileOutputStream;
  }

  @Override
  public void close() throws IOException {
    this.fileOutputStream.close();
  }

  /**
   * Returns a TestResultsOutputSender object if the default environment variable is set.
   *
   * @return An optional TestResultsOutputSender object.
   */
  public static Optional<TestResultsOutputSender> fromDefaultEnvName() {
    return fromEnvName(TEST_RESULTS_OUTPUT_FILE_ENV_NAME);
  }

  @SuppressWarnings("PMD.BlacklistedSystemGetenv")
  private static String getEnvValue(String envName) {
    return System.getenv(envName);
  }

  /**
   * Returns a TestResultsOutputSender object if the given environment variable is set.
   *
   * @param envName The name of the environment variable to check.
   * @return An optional TestResultsOutputSender object.
   */
  public static Optional<TestResultsOutputSender> fromEnvName(String envName) {
    return fromFilePath(getEnvValue(envName));
  }

  /**
   * Creates a TestResultsOutputSender object from a file path.
   *
   * @param testResultsOutputFile The file path to create the TestResultsOutputSender from.
   * @return An optional TestResultsOutputSender object.
   */
  public static Optional<TestResultsOutputSender> fromFilePath(String testResultsOutputFile) {
    if (testResultsOutputFile == null) {
      return Optional.empty();
    }

    try {
      FileOutputStream fileOutputStream = new FileOutputStream(testResultsOutputFile);

      TestResultsOutputSender testResultsOutputSender =
          new TestResultsOutputSender(fileOutputStream);

      return Optional.of(testResultsOutputSender);
    } catch (FileNotFoundException e) {
      throw new RuntimeException(e);
    }
  }

  /**
   * Sends a test start event to the output file.
   *
   * @param name The name of the test.
   */
  public void sendTestStart(String name) {
    StartEvent startEvent = new StartEvent(name);

    byte[] serialized;
    try {
      serialized = startEvent.toJsonBytes();
    } catch (IOException e) {
      throw new RuntimeException(e);
    }

    try {
      this.fileOutputStream.write(serialized);
      this.fileOutputStream.write("\n".getBytes());
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }

  /**
   * Sends a test finish event to the output file.
   *
   * @param name The name of the test.
   * @param status The status of the test.
   * @param endedTime The time the test ended.
   * @param duration The duration of the test.
   * @param message An optional message associated with the test.
   */
  public void sendTestFinish(
      String name, TestStatus status, long endedTime, long duration, Optional<String> message) {

    FinishEvent finishEvent = new FinishEvent(name, status, endedTime, duration, message);

    byte[] serialized;
    try {
      serialized = finishEvent.toJsonBytes();
    } catch (IOException e) {
      throw new RuntimeException(e);
    }

    try {
      this.fileOutputStream.write(serialized);
      this.fileOutputStream.write("\n".getBytes());
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }
}
