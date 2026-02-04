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
import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.junit.runner.Description;
import org.junit.runner.notification.Failure;

/** Tests {@link JUnitTpxStandardOutputListener} */
public class JUnitTpxStandardOutputTestListenerTest {
  File tempFile;

  @Before
  public void setUp() throws IOException {
    tempFile = folder.newFile("test_results.json");
  }

  public JUnitTpxStandardOutputListener createListener(FileOutputStream fileOutputStream) {
    return new JUnitTpxStandardOutputListener(new TestResultsOutputSender(fileOutputStream));
  }

  @Rule public TemporaryFolder folder = new TemporaryFolder();

  @Test
  public void testPassedTest() throws IOException {
    try (FileOutputStream fileOutputStream = new FileOutputStream(tempFile)) {
      JUnitTpxStandardOutputListener listener = createListener(fileOutputStream);

      Description description = Description.createTestDescription("TestClass", "testOne");
      listener.testStarted(description);
      listener.testFinished(description);
    }

    try (BufferedReader reader = new BufferedReader(new FileReader(tempFile))) {
      String startLine = reader.readLine();
      Assert.assertTrue(startLine.contains("start"));
      Assert.assertTrue(startLine.contains("testOne (TestClass)"));

      String endLine = reader.readLine();
      Assert.assertTrue(endLine.contains("finish"));
      Assert.assertTrue(endLine.contains("testOne (TestClass)"));

      Assert.assertNull(reader.readLine());
    }
  }

  @Test
  public void testFailedTest() throws IOException {
    String testFailureMessage = "failed test trace example";
    try (FileOutputStream fileOutputStream = new FileOutputStream(tempFile)) {
      JUnitTpxStandardOutputListener listener = createListener(fileOutputStream);

      Description description = Description.createTestDescription("TestClass", "testOne");
      listener.testStarted(description);
      Failure testFailure = new Failure(description, new Throwable(testFailureMessage));
      listener.testFailure(testFailure);
      listener.testFinished(description);
    }

    try (BufferedReader reader = new BufferedReader(new FileReader(tempFile))) {
      String startLine = reader.readLine();
      Assert.assertTrue(startLine.contains("start"));
      Assert.assertTrue(startLine.contains("testOne (TestClass)"));

      String endLine = reader.readLine();
      Assert.assertTrue(endLine.contains("finish"));
      Assert.assertTrue(endLine.contains("testOne (TestClass)"));
      Assert.assertTrue(endLine.contains(testFailureMessage));
      Assert.assertTrue(endLine.contains("\\n")); // stack trace contains new lines

      Assert.assertNull(reader.readLine());
    }
  }

  @Test
  public void testFailedAssumptionTest() throws IOException {
    String testFailureMessage = "failed test trace example";
    try (FileOutputStream fileOutputStream = new FileOutputStream(tempFile)) {
      JUnitTpxStandardOutputListener listener = createListener(fileOutputStream);
      Description description = Description.createTestDescription("TestClass", "testOne");
      listener.testStarted(description);
      Failure testFailure = new Failure(description, new Throwable(testFailureMessage));
      listener.testAssumptionFailure(testFailure);
      listener.testFinished(description);
    }

    try (BufferedReader reader = new BufferedReader(new FileReader(tempFile))) {
      String startLine = reader.readLine();
      Assert.assertTrue(startLine.contains("start"));
      Assert.assertTrue(startLine.contains("testOne (TestClass)"));

      String endLine = reader.readLine();
      Assert.assertTrue(endLine.contains("finish"));
      Assert.assertTrue(endLine.contains("testOne (TestClass)"));
      Assert.assertTrue(endLine.contains(testFailureMessage));
      Assert.assertTrue(endLine.contains("\\n")); // stack trace contains new lines

      Assert.assertNull(reader.readLine());
    }
  }

  @Test
  public void testIgnoredTest() throws IOException {
    try (FileOutputStream fileOutputStream = new FileOutputStream(tempFile)) {
      JUnitTpxStandardOutputListener listener = createListener(fileOutputStream);

      Description description = Description.createTestDescription("TestClass", "testOne");
      listener.testStarted(description);
      listener.testIgnored(description);
      listener.testFinished(description);
    }

    try (BufferedReader reader = new BufferedReader(new FileReader(tempFile))) {
      String startLine = reader.readLine();
      Assert.assertTrue(startLine.contains("start"));
      Assert.assertTrue(startLine.contains("testOne (TestClass)"));

      String endLine = reader.readLine();
      Assert.assertTrue(endLine.contains("finish"));
      Assert.assertTrue(endLine.contains("testOne (TestClass)"));
      Assert.assertTrue(endLine.contains("Test ignored"));

      Assert.assertNull(reader.readLine());
    }
  }

  @Test
  public void testNoTestsRemainExceptionIsIgnored() throws IOException {
    // When all tests are filtered out (e.g., TPX retries only @Ignore tests),
    // JUnit fires testStarted/testFailure/testFinished for an "initializationError"
    // with className "org.junit.runner.manipulation.Filter". This should be ignored
    // and not reported to TPX.
    try (FileOutputStream fileOutputStream = new FileOutputStream(tempFile)) {
      JUnitTpxStandardOutputListener listener = createListener(fileOutputStream);

      Description description =
          Description.createTestDescription(
              "org.junit.runner.manipulation.Filter", "initializationError");
      listener.testStarted(description);
      Failure testFailure =
          new Failure(
              description,
              new Exception(
                  "No tests found matching TestSelectorList-filter from"
                      + " org.junit.runner.Request$1@86733"));
      listener.testFailure(testFailure);
      listener.testFinished(description);
    }

    // The file should be empty - no events should have been sent
    try (BufferedReader reader = new BufferedReader(new FileReader(tempFile))) {
      Assert.assertNull("Expected no events for NoTestsRemainException", reader.readLine());
    }
  }
}
