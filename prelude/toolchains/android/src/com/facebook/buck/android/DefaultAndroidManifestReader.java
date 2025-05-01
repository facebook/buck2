/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android;

import com.facebook.buck.util.xml.XmlDomParser;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import javax.xml.namespace.NamespaceContext;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

public class DefaultAndroidManifestReader implements AndroidManifestReader {

  /**
   * XPath expression to retrieve the names of activities with an intent-filter that gets them to
   * show up in the launcher.
   */
  private static final String XPATH_LAUNCHER_ACTIVITIES =
      "/manifest/application/*  [self::activity[not(@android:enabled) or @android:enabled='true']"
          + " or                  self::activity-alias[not(@android:enabled) or"
          + " @android:enabled='true']] "
          + " [intent-filter[action/@android:name='android.intent.action.MAIN' and                 "
          + " category/@android:name='android.intent.category.LAUNCHER']]  /@android:name";

  /**
   * XPath expression to get the package. For a manifest as {@code <manifest
   * package="com.facebook.katana" />}, this results in {@code com.facebook.katana}.
   */
  private static final String XPATH_PACKAGE = "/manifest/@package";

  /**
   * XPath expression to get the version code. For a manifest as {@code <manifest
   * android:versionCode="1" />}, this results in {@code 1}.
   */
  private static final String XPATH_VERSION_CODE = "/manifest/@android:versionCode";

  /** XPath expression to get the instrumentation test runner. */
  private static final String XPATH_INSTRUMENTATION_TEST_RUNNER =
      "/manifest/instrumentation/@android:name";

  /** XPath expression to get the target package. */
  private static final String XPATH_TARGET_PACKAGE =
      "/manifest/instrumentation/@android:targetPackage";

  private final XPathExpression packageExpression;
  private final XPathExpression versionCodeExpression;
  private final XPathExpression instrumentationTestRunnerExpression;
  private final XPathExpression targetPackageExpression;
  private final XPathExpression launchableActivitiesExpression;
  private final Document doc;

  private DefaultAndroidManifestReader(InputSource src) throws IOException {
    try {
      // Parse the XML.
      doc = XmlDomParser.parse(src, true);

      // Compile the XPath expressions.
      XPath xPath = XPathFactory.newInstance().newXPath();
      xPath.setNamespaceContext(androidNamespaceContext);
      launchableActivitiesExpression = xPath.compile(XPATH_LAUNCHER_ACTIVITIES);
      packageExpression = xPath.compile(XPATH_PACKAGE);
      targetPackageExpression = xPath.compile(XPATH_TARGET_PACKAGE);
      versionCodeExpression = xPath.compile(XPATH_VERSION_CODE);
      instrumentationTestRunnerExpression = xPath.compile(XPATH_INSTRUMENTATION_TEST_RUNNER);
    } catch (XPathExpressionException | SAXException e) {
      throw new RuntimeException(e);
    }
  }

  @Override
  public List<String> getLauncherActivities() {
    try {
      NodeList nodes;
      nodes = (NodeList) launchableActivitiesExpression.evaluate(doc, XPathConstants.NODESET);

      List<String> activities = new ArrayList<>();
      for (int i = 0; i < nodes.getLength(); i++) {
        activities.add(nodes.item(i).getTextContent());
      }
      return activities;

    } catch (XPathExpressionException e) {
      throw new RuntimeException(e);
    }
  }

  @Override
  public String getPackage() {
    try {
      return (String) packageExpression.evaluate(doc, XPathConstants.STRING);
    } catch (XPathExpressionException e) {
      throw new RuntimeException(e);
    }
  }

  @Override
  public String getVersionCode() {
    try {
      return (String) versionCodeExpression.evaluate(doc, XPathConstants.STRING);
    } catch (XPathExpressionException e) {
      throw new RuntimeException(e);
    }
  }

  @Override
  public String getInstrumentationTestRunner() {
    try {
      return (String) instrumentationTestRunnerExpression.evaluate(doc, XPathConstants.STRING);
    } catch (XPathExpressionException e) {
      throw new RuntimeException(e);
    }
  }

  @Override
  public String getTargetPackage() {
    try {
      return (String) targetPackageExpression.evaluate(doc, XPathConstants.STRING);
    } catch (XPathExpressionException e) {
      throw new RuntimeException(e);
    }
  }

  /** This allows querying the AndroidManifest for e.g. attributes like android:name using XPath */
  private static NamespaceContext androidNamespaceContext =
      new NamespaceContext() {
        @Override
        public Iterator<String> getPrefixes(String namespaceURI) {
          throw new UnsupportedOperationException();
        }

        @Override
        public String getPrefix(String namespaceURI) {
          throw new UnsupportedOperationException();
        }

        @Override
        public String getNamespaceURI(String prefix) {
          if (prefix.equals("android")) {
            return "http://schemas.android.com/apk/res/android";
          } else {
            throw new IllegalArgumentException();
          }
        }
      };

  /**
   * Parses an XML given via its path and returns an {@link AndroidManifestReader} for it.
   *
   * @param path path to an AndroidManifest.xml file
   * @return an {@code AndroidManifestReader} for {@code path}
   * @throws IOException
   */
  public static AndroidManifestReader forPath(Path path) throws IOException {
    try (Reader reader = Files.newBufferedReader(path)) {
      return forReader(reader);
    }
  }

  /**
   * Parses an XML given as a string and returns an {@link AndroidManifestReader} for it.
   *
   * @param xmlString a string representation of an XML document
   * @return an {@code AndroidManifestReader} for the XML document
   * @throws IOException
   */
  public static AndroidManifestReader forString(String xmlString) throws IOException {
    return forReader(new StringReader(xmlString));
  }

  private static AndroidManifestReader forReader(Reader reader) throws IOException {
    return new DefaultAndroidManifestReader(new InputSource(reader));
  }
}
