/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.util.xml;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.w3c.dom.Document;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

public class XmlDomParser {

  /** Utility class: do not instantiate. */
  private XmlDomParser() {}

  public static Document parse(Path xml) throws IOException, SAXException {
    try (InputStream is = Files.newInputStream(xml)) {
      return parse(is);
    }
  }

  public static Document parse(String xmlContents) throws IOException, SAXException {
    return parse(new ByteArrayInputStream(xmlContents.getBytes(StandardCharsets.UTF_8)));
  }

  public static Document parse(InputStream stream) throws IOException, SAXException {
    return parse(new InputSource(stream), /* namespaceAware */ false);
  }

  public static Document parse(InputSource xml, boolean namespaceAware)
      throws IOException, SAXException {
    DocumentBuilder docBuilder;
    try {
      DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
      if (namespaceAware) {
        factory.setNamespaceAware(namespaceAware);
      }
      docBuilder = factory.newDocumentBuilder();
    } catch (ParserConfigurationException e) {
      throw new RuntimeException(e);
    }

    return docBuilder.parse(xml);
  }
}
