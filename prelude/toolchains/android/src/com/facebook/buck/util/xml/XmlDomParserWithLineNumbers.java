/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.util.xml;

import java.io.IOException;
import java.io.InputStream;
import java.util.Objects;
import javax.annotation.Nullable;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import org.w3c.dom.DOMImplementation;
import org.w3c.dom.Document;
import org.xml.sax.EntityResolver;
import org.xml.sax.ErrorHandler;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

public class XmlDomParserWithLineNumbers {
  /** Utility class: do not instantiate. */
  private XmlDomParserWithLineNumbers() {}

  public static Document parse(InputStream xml) throws IOException, SAXException {
    return parse(new InputSource(xml));
  }

  /**
   * Constructs a document builder, parses xml, then returns a document with a new DocumentLocation
   * object stored as userData in each node
   *
   * @param xml the input source of xml
   * @return a document of parsed xml with line and column data in each node
   * @throws IOException
   * @throws SAXException
   */
  public static Document parse(InputSource xml) throws IOException, SAXException {
    DocumentBuilderWithLineNumbers docBuilder = new DocumentBuilderWithLineNumbers();
    return docBuilder.parse(xml);
  }

  static class DocumentBuilderWithLineNumbers extends DocumentBuilder {

    @Override
    public Document parse(InputSource is) throws SAXException, IOException {
      SAXParser parser;
      try {
        SAXParserFactory factory = SAXParserFactory.newInstance();
        parser = factory.newSAXParser();

        PositionalXmlHandler xmlHandler = new PositionalXmlHandler();
        parser.parse(is, xmlHandler);
        Document doc = xmlHandler.getDocument();
        Objects.requireNonNull(doc);
        return doc;
      } catch (ParserConfigurationException e) {
        throw new RuntimeException("Can't create SAX parser / DOM builder.", e);
      }
    }

    @Override
    public boolean isNamespaceAware() {
      return false;
    }

    @Override
    public boolean isValidating() {
      return false;
    }

    @Override
    public void setEntityResolver(EntityResolver er) {}

    @Override
    public void setErrorHandler(ErrorHandler eh) {}

    @Nullable
    @Override
    public Document newDocument() {
      return null;
    }

    @Nullable
    @Override
    public DOMImplementation getDOMImplementation() {
      return null;
    }
  }
}
