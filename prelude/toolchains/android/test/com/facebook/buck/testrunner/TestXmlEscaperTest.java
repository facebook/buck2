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

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class TestXmlEscaperTest {

  @Test
  public void contentEscaperReplacesCharacters() {

    String inputStr =
        "\uFFFE" // out of bounds
            + (char) 0x00 // ascii control character to be replaced
            + 'h' // not replaced
            + '&' // replaced
            + '<' // replaced
            + '1' // not replaced
            + '>' // replaced
        ;

    String outputStr =
        "�" // out of bounds
            + '\uFFFD' // ascii control character to be replaced
            + 'h'
            + "&amp;"
            + "&lt;"
            + '1'
            + "&gt;";

    assertEquals(outputStr, TestXmlEscaper.CONTENT_ESCAPER.escape(inputStr));
  }

  @Test
  public void attributeEscaperReplacesCharacters() {

    String inputStr =
        "\uFFFE" // out of bounds
            + (char) 0x00 // ascii control character to be replaced
            + 'h' // not replaced
            + '&' // replaced
            + '<' // replaced
            + '1' // not replaced
            + '>' // replaced
            + '\'' // replaced
            + '"' // replaced
            + '\t' // replaced
            + '!' // not replaced
            + '\n' // replaced
            + '\r' // replaced
        ;

    String outputStr =
        "�" // out of bounds
            + '\uFFFD' // ascii control character to be replaced
            + 'h'
            + "&amp;"
            + "&lt;"
            + '1'
            + "&gt;"
            + "&apos;"
            + "&quot;"
            + "&#x9;"
            + '!'
            + "&#xA;"
            + "&#xD;";

    assertEquals(outputStr, TestXmlEscaper.ATTRIBUTE_ESCAPER.escape(inputStr));
  }
}
