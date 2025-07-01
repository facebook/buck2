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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import org.junit.Test;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

public class XmlDomParserTest {

  private static class StringReaderForCloseCheck extends StringReader {
    private boolean isClosed = false;

    public StringReaderForCloseCheck(String s) {
      super(s);
    }

    @Override
    public void close() {
      super.close();
      isClosed = true;
    }

    public boolean isClosed() {
      return isClosed;
    }
  }

  /**
   * Checks that when creating an {@link InputSource} from a {@link Reader} and passing that through
   * {@link com.facebook.buck.util.xml.XmlDomParser#parse(InputSource,boolean)}, it is closed before
   * the method returns.
   *
   * @see <a href="http://fburl.com/8289364">DocumentBuilder.parse(InputStream)</a>
   * @throws IOException
   */
  @Test
  public void testXmlDomParserClosesReader() throws IOException, SAXException {
    StringReaderForCloseCheck reader =
        new StringReaderForCloseCheck("<?xml version='1.0'?> <a><b><c></c></b></a>");
    assertFalse(reader.isClosed());
    XmlDomParser.parse(new InputSource(reader), false);
    assertTrue(reader.isClosed());
  }
}
