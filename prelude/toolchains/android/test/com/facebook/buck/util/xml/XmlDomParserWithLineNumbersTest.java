/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.util.xml;

import static org.junit.Assert.assertEquals;

import com.facebook.buck.testutil.integration.TestDataHelper;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.Before;
import org.junit.Test;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;

public class XmlDomParserWithLineNumbersTest {

  private Path testDataDir;

  @Before
  public void setUp() throws IOException {
    testDataDir = TestDataHelper.getTestDataDirectory(this);
  }

  /**
   * Checks that when creating an {@link ByteArrayInputStream} and passing that through {@link
   * XmlDomParserWithLineNumbers#parse}, the resulting document contains correctly set line numbers
   *
   * @see <a href="http://fburl.com/8289364">DocumentBuilder.parse(InputStream)</a>
   * @throws IOException, SAXException
   */
  @Test
  public void testXmlDomParsesLineNumbersCorrectly() throws IOException, SAXException {
    Document dom =
        XmlDomParserWithLineNumbers.parse(Files.newInputStream(testDataDir.resolve("sample.xml")));

    DocumentLocation documentLocation =
        (DocumentLocation)
            dom.getDocumentElement()
                .getFirstChild()
                .getNextSibling()
                .getUserData(PositionalXmlHandler.LOCATION_USER_DATA_KEY);
    assertEquals(DocumentLocation.of(10, 43), documentLocation);
  }
}
