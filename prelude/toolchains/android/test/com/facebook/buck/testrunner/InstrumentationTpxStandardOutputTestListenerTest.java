/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.testrunner;

import com.android.ddmlib.IDevice;
import com.android.ddmlib.testrunner.TestIdentifier;
import com.facebook.buck.android.TestDevice;
import com.facebook.buck.testresultsoutput.TestResultsOutputSender;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

// We basically just want to test that the listener doesn't throw IllegalStateExceptions when it is
// given a correct order of events and that it outputs the same count of lines.
/** Tests {@link InstrumentationTpxStandardOutputTestListener} */
public class InstrumentationTpxStandardOutputTestListenerTest {
  IDevice testDevice;
  File tempFile;

  @Before
  public void setUp() throws IOException {
    testDevice = new TestDevice();
    tempFile = folder.newFile("test_results.json");
  }

  public InstrumentationTpxStandardOutputTestListener createListener(
      FileOutputStream fileOutputStream) {
    return new InstrumentationTpxStandardOutputTestListener(
        new TestResultsOutputSender(fileOutputStream), testDevice);
  }

  @Rule public TemporaryFolder folder = new TemporaryFolder();

  @Test
  public void testPassedTest() throws IOException {
    try (FileOutputStream fileOutputStream = new FileOutputStream(tempFile)) {
      InstrumentationTpxStandardOutputTestListener listener = createListener(fileOutputStream);

      TestIdentifier testIdentifier = new TestIdentifier("TestClass", "testOne");
      listener.testStarted(testIdentifier);
      listener.testEnded(testIdentifier, new HashMap<String, String>());
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
      InstrumentationTpxStandardOutputTestListener listener = createListener(fileOutputStream);

      TestIdentifier testIdentifier = new TestIdentifier("TestClass", "testOne");
      listener.testStarted(testIdentifier);
      listener.testFailed(testIdentifier, testFailureMessage);
      listener.testEnded(testIdentifier, new HashMap<String, String>());
    }

    try (BufferedReader reader = new BufferedReader(new FileReader(tempFile))) {
      String startLine = reader.readLine();
      Assert.assertTrue(startLine.contains("start"));
      Assert.assertTrue(startLine.contains("testOne (TestClass)"));

      String endLine = reader.readLine();
      Assert.assertTrue(endLine.contains("finish"));
      Assert.assertTrue(endLine.contains("testOne (TestClass)"));
      Assert.assertTrue(endLine.contains(testFailureMessage));

      Assert.assertNull(reader.readLine());
    }
  }

  @Test
  public void testFailedAssumptionTest() throws IOException {
    String testFailureMessage = "failed test trace example";
    try (FileOutputStream fileOutputStream = new FileOutputStream(tempFile)) {
      InstrumentationTpxStandardOutputTestListener listener = createListener(fileOutputStream);

      TestIdentifier testIdentifier = new TestIdentifier("TestClass", "testOne");
      listener.testStarted(testIdentifier);
      listener.testAssumptionFailure(testIdentifier, testFailureMessage);
      listener.testEnded(testIdentifier, new HashMap<String, String>());
    }

    try (BufferedReader reader = new BufferedReader(new FileReader(tempFile))) {
      String startLine = reader.readLine();
      Assert.assertTrue(startLine.contains("start"));
      Assert.assertTrue(startLine.contains("testOne (TestClass)"));

      String endLine = reader.readLine();
      Assert.assertTrue(endLine.contains("finish"));
      Assert.assertTrue(endLine.contains("testOne (TestClass)"));
      Assert.assertTrue(endLine.contains(testFailureMessage));

      Assert.assertNull(reader.readLine());
    }
  }

  @Test
  public void testIgnoredTest() throws IOException {
    try (FileOutputStream fileOutputStream = new FileOutputStream(tempFile)) {
      InstrumentationTpxStandardOutputTestListener listener = createListener(fileOutputStream);

      TestIdentifier testIdentifier = new TestIdentifier("TestClass", "testOne");
      listener.testStarted(testIdentifier);
      listener.testIgnored(testIdentifier);
      listener.testEnded(testIdentifier, new HashMap<String, String>());
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
}
