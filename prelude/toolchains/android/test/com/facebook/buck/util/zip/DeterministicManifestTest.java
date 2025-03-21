/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.util.zip;

import static org.hamcrest.Matchers.lessThanOrEqualTo;
import static org.junit.Assert.assertArrayEquals;

import com.facebook.buck.jvm.java.version.utils.JavaVersionUtils;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.jar.Attributes;
import java.util.jar.Manifest;
import org.junit.Assume;
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

  @Test
  public void testLinesSplitLikeJavaImpl() throws IOException {
    // TODO(jtorkkola): Re-enable and change behavior of {@link DeterministicManifest} to match new
    //  behavior {@link java.util.jar.Manifest} once Java 11 migration is complete.
    Assume.assumeThat(JavaVersionUtils.getMajorVersion(), lessThanOrEqualTo(8));

    String entryName = "test";
    String key = "12345678";
    String value =
        "138-char value + 8 char key + 2 char padding = 148 chars.  |"
            + "69-character second line                                            |"
            + "last line";

    manifestWriter.setEntryAttribute(entryName, key, value);
    manifestWriter.write(outputStream);

    Manifest jdkManifest = new Manifest();
    Attributes attrs = new Attributes();
    attrs.putValue(key, value);
    jdkManifest.getEntries().put(entryName, attrs);

    ByteArrayOutputStream expected = new ByteArrayOutputStream();
    jdkManifest.write(expected);

    assertArrayEquals(expected.toByteArray(), outputStream.toByteArray());
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
