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
import com.android.ddmlib.testrunner.TestIdentifier;
import com.android.ddmlib.testrunner.XmlTestRunListener;
import java.io.File;
import java.io.IOException;
import java.util.Map;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.SAXException;

public class BuckXmlTestRunListener extends XmlTestRunListener {

  protected static final String TEST_RESULT_FILE = "test_result.xml";
  private String mRunFailureMessage = null;
  private File mReportDir;
  private IDevice mDevice;

  BuckXmlTestRunListener(IDevice device) {
    mDevice = device;
  }

  @Override
  public void setReportDir(File file) {
    super.setReportDir(file);
    mReportDir = file;
  }

  @Override
  public void testRunStarted(String runName, int testCount) {
    super.testRunStarted(runName, testCount);
  }

  @Override
  public void testRunFailed(String errorMessage) {
    mRunFailureMessage = errorMessage;
  }

  @Override
  public void testRunEnded(long elapsedTime, Map<String, String> runMetrics) {
    super.testRunEnded(elapsedTime, runMetrics);
    addMainTestResult();
  }

  @Override
  public void testFailed(TestIdentifier test, String trace) {
    if (mDevice != null && CrashCapturer.deviceHasCrashLogs(trace)) {
      trace = CrashCapturer.addDeviceLogcatTrace(mDevice, trace);
    }
    super.testFailed(test, trace);
  }

  /** Adds one more XML element to the test_result.xml tracking the result of the whole process. */
  private void addMainTestResult() {
    try {
      File resultFile = getResultFile(mReportDir);

      DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
      DocumentBuilder docBuilder = docFactory.newDocumentBuilder();
      Document doc = docBuilder.parse(resultFile);

      Node testsuite = doc.getElementsByTagName("testsuite").item(0);
      if (mRunFailureMessage != null) {
        Element failureNode = doc.createElement("failure");
        failureNode.setTextContent(mRunFailureMessage);
        testsuite.appendChild(failureNode);

        TransformerFactory transformerFactory = TransformerFactory.newInstance();
        Transformer transformer = transformerFactory.newTransformer();
        DOMSource source = new DOMSource(doc);
        StreamResult result = new StreamResult(resultFile);
        transformer.transform(source, result);
      }

    } catch (IOException | ParserConfigurationException | SAXException | TransformerException e) {
      throw new RuntimeException(e);
    }
  }

  /**
   * Creates a {@link File} where the report will be created. Instead of creating a temp file,
   * create a file with the devices serial number, so it's possible to refer back to it afterwards.
   *
   * @param reportDir the root directory of the report.
   * @return a file
   * @throws IOException
   */
  @Override
  protected File getResultFile(File reportDir) {
    return new File(reportDir, TEST_RESULT_FILE);
  }
}
