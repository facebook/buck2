/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.util.xml;

import com.google.common.annotations.VisibleForTesting;
import java.util.Objects;
import java.util.Stack;
import javax.annotation.Nullable;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.Attributes;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.ext.Locator2;
import org.xml.sax.helpers.DefaultHandler;

/** Builds a DOM tree that maintains element line and column numbers in userData */
public class PositionalXmlHandler extends DefaultHandler {
  public static final String LOCATION_USER_DATA_KEY = "lineLocation";

  @Nullable private Document document;

  @Nullable private Locator2 locator;

  private final Stack<Element> elementStack = new Stack<>();
  private StringBuilder textBuffer = new StringBuilder();

  @Override
  public void startDocument() {
    try {
      DocumentBuilderFactory builderFactory = DocumentBuilderFactory.newInstance();
      DocumentBuilder documentBuilder = builderFactory.newDocumentBuilder();
      document = documentBuilder.newDocument();
    } catch (ParserConfigurationException ex) {
      throw new RuntimeException("Cannot create document", ex);
    }
  }

  @Override
  public void setDocumentLocator(Locator locator) {
    this.locator = (Locator2) locator;
  }

  @Override
  public void startElement(String uri, String localName, String qName, Attributes attributes) {
    addText();
    Objects.requireNonNull(document);
    Element element = document.createElementNS(uri, qName);
    for (int i = 0; i < attributes.getLength(); i++) {
      element.setAttribute(attributes.getQName(i), attributes.getValue(i));
    }
    element.setUserData(LOCATION_USER_DATA_KEY, getDocumentLocation(), null);
    elementStack.push(element);
  }

  @Override
  public void endElement(String uri, String localName, String qName) {
    addText();
    Element closedElement = elementStack.pop();
    if (elementStack.isEmpty()) { // If this is the root element
      closedElement.setUserData("encoding", getLocatorEncoding(), null);
      Objects.requireNonNull(document);
      document.appendChild(closedElement);
    } else {
      Element parentElement = elementStack.peek();
      parentElement.appendChild(closedElement);
    }
  }

  @Override
  public void characters(char[] ch, int start, int length) {
    textBuffer.append(ch, start, length);
  }

  /** Returns the text inside the current tag */
  @VisibleForTesting
  void addText() {
    if (textBuffer.length() > 0) {
      Objects.requireNonNull(document);
      Element element = elementStack.peek();
      Node textNode = document.createTextNode(textBuffer.toString());
      element.appendChild(textNode);
      textBuffer = new StringBuilder();
    }
  }

  @Nullable
  private DocumentLocation getDocumentLocation() {
    if (locator == null) {
      return null;
    }

    return DocumentLocation.of(locator.getLineNumber() - 1, locator.getColumnNumber() - 1);
  }

  @Nullable
  private String getLocatorEncoding() {
    if (locator == null) {
      return null;
    }

    return locator.getEncoding();
  }

  @Nullable
  Document getDocument() {
    return document;
  }

  @Override
  public void error(SAXParseException ex) throws SAXException {
    // nests ex to conserve exception line number
    throw new SAXException(ex.getMessage(), ex);
  }

  @Override
  public void fatalError(SAXParseException ex) throws SAXException {
    throw new SAXException(ex.getMessage(), ex);
  }

  @Override
  public void warning(SAXParseException exception) { // do nothing
  }
}
