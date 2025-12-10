/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.util.zip;

import static org.junit.Assert.assertArrayEquals;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.jar.Attributes;
import org.junit.Before;
import org.junit.Test;

public class DeterministicManifestTest {

  private DeterministicManifest manifestWriter;
  private ByteArrayOutputStream outputStream;

  @Before
  public void setUp() {
    outputStream = new ByteArrayOutputStream();
    manifestWriter = new DeterministicManifest();
  }

  @Test
  public void testManifestAttributesSeparatedFromEntries() throws IOException {
    manifestWriter.setManifestAttribute(Attributes.Name.MANIFEST_VERSION.toString(), "1.0");
    manifestWriter.setEntryAttribute("Z", "Foo", "Bar");
    manifestWriter.setEntryAttribute("A", "Foo", "Bar");
    manifestWriter.write(outputStream);

    assertManifestContents(
        "Manifest-Version: 1.0\r\n"
            + "\r\n"
            + "Name: A\r\n"
            + "Foo: Bar\r\n"
            + "\r\n"
            + "Name: Z\r\n"
            + "Foo: Bar\r\n"
            + "\r\n");
  }

  @Test
  public void testEntriesWrittenInSortedOrder() throws IOException {
    manifestWriter.setEntryAttribute("Z", "Foo", "Bar");
    manifestWriter.setEntryAttribute("A", "Foo", "Bar");
    manifestWriter.write(outputStream);

    assertManifestContents(
        "\r\n" + "Name: A\r\n" + "Foo: Bar\r\n" + "\r\n" + "Name: Z\r\n" + "Foo: Bar\r\n" + "\r\n");
  }

  @Test
  public void testAttributesWrittenInSortedOrder() throws IOException {
    manifestWriter.setEntryAttribute("A", "Foo", "Bar");
    manifestWriter.setEntryAttribute("A", "Baz", "Bar");
    manifestWriter.write(outputStream);

    assertManifestContents("\r\n" + "Name: A\r\n" + "Baz: Bar\r\n" + "Foo: Bar\r\n" + "\r\n");
  }

  @Test
  public void testShortLinesWrittenOnOneLine() throws IOException {
    assertEntryWrittenAs("\r\n" + "Name: Entry\r\n" + "Key: value\r\n" + "\r\n", "Key", "value");
  }

  @Test
  public void testLongLineSplit() throws IOException {
    assertEntryWrittenAs(
        "\r\n"
            + "Name: Entry\r\n"
            + "12345678: 69-char value + 8 char key + 2 char padding = 79 chars.    |\r\n"
            + " next line\r\n"
            + "\r\n",
        "12345678",
        "69-char value + 8 char key + 2 char padding = 79 chars.    |" + "next line");
  }

  @Test
  public void testReallyLongLineSplit() throws IOException {
    assertEntryWrittenAs(
        "\r\n"
            + "Name: Entry\r\n"
            + "12345678: 138-char value + 8 char key + 2 char padding = 148 chars.  |\r\n"
            + " 69-character second line                                            |\r\n"
            + " last line\r\n"
            + "\r\n",
        "12345678",
        "138-char value + 8 char key + 2 char padding = 148 chars.  |"
            + "69-character second line                                            |"
            + "last line");
  }

  @Test
  public void testReallyLongLineSplitExact() throws IOException {
    assertEntryWrittenAs(
        "\r\n"
            + "Name: Entry\r\n"
            + "12345678: 138-char value + 8 char key + 2 char padding = 148 chars.  |\r\n"
            + " 69-character second line                                            |\r\n"
            + "\r\n",
        "12345678",
        "138-char value + 8 char key + 2 char padding = 148 chars.  |"
            + "69-character second line                                            |");
  }

  @Test
  public void testReallyLongLineSplitOneExtra() throws IOException {
    assertEntryWrittenAs(
        "\r\n"
            + "Name: Entry\r\n"
            + "12345678: 138-char value + 8 char key + 2 char padding = 148 chars.  |\r\n"
            + " 69-character second line                                            |\r\n"
            + " X\r\n"
            + "\r\n",
        "12345678",
        "138-char value + 8 char key + 2 char padding = 148 chars.  |"
            + "69-character second line                                            |"
            + "X");
  }

  private void assertEntryWrittenAs(String expected, String key, String value) throws IOException {
    manifestWriter.setEntryAttribute("Entry", key, value);
    manifestWriter.write(outputStream);

    assertManifestContents(expected);
  }

  private void assertManifestContents(String expected) {
    assertArrayEquals(expected.getBytes(StandardCharsets.UTF_8), outputStream.toByteArray());
  }
}
