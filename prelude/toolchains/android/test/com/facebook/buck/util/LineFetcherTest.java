/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.io.StringReader;
import org.junit.Test;

public class LineFetcherTest {

  private static final String TEST_LINE_1 = "Test1";
  private static final String TEST_LINE_2 = "Test2";

  @Test
  public void testUnterminateLineIsReadAsSingleLine() throws Exception {
    String[] expected = {TEST_LINE_1};
    verifyLineFetcherResponses(TEST_LINE_1, expected);
  }

  @Test
  public void testNewLineLineEnding() throws Exception {
    verifyLineEndingBehaviour("\n");
  }

  @Test
  public void testCarriageReturnLineEnding() throws Exception {
    verifyLineEndingBehaviour("\r");
  }

  @Test
  public void testDOSLineEndingIsTreatedAsOneLineEnding() throws Exception {
    verifyLineEndingBehaviour("\r\n");
  }

  private void verifyLineEndingBehaviour(String lineEnding) throws Exception {
    String[] expected = {TEST_LINE_1, TEST_LINE_2};
    verifyLineFetcherResponses(TEST_LINE_1 + lineEnding + TEST_LINE_2, expected);
  }

  @Test
  public void testBufferSizedLine() throws Exception {
    verifyLineEndingAtSpecificPlace(LineFetcher.BUFFER_LENGTH, "\r");
  }

  @Test
  public void testLineOverBufferSize() throws Exception {
    verifyLineEndingAtSpecificPlace(LineFetcher.BUFFER_LENGTH + 1, "\r");
  }

  @Test
  public void testDOSLineEndingStartingOnBuffersEdge() throws Exception {
    verifyLineEndingAtSpecificPlace(LineFetcher.BUFFER_LENGTH - 1, "\r\n");
  }

  @Test
  public void testDoubleBufferSizeLineLength() throws Exception {
    verifyLineEndingAtSpecificPlace(LineFetcher.BUFFER_LENGTH * 2, "\r");
  }

  @Test
  public void testTrippleBufferSizeLineLength() throws Exception {
    verifyLineEndingAtSpecificPlace(LineFetcher.BUFFER_LENGTH * 3, "\r");
  }

  private void verifyLineEndingAtSpecificPlace(int lineEndLocation, String lineEnding)
      throws Exception {
    StringBuilder builder =
        new StringBuilder(lineEndLocation + lineEnding.length() + TEST_LINE_2.length());
    for (int i = 0; i < lineEndLocation; i++) {
      builder.append('X');
    }

    String[] expected = {builder.toString(), TEST_LINE_2};

    builder.append(lineEnding);
    builder.append(TEST_LINE_2);

    verifyLineFetcherResponses(builder.toString(), expected);
  }

  private void verifyLineFetcherResponses(String sample, String[] expectedResponses)
      throws Exception {
    try (LineFetcher fetcher = new LineFetcher(new StringReader(sample))) {
      for (String expectedRespons : expectedResponses) {
        assertEquals(
            "Mismatch in expected and actual response from LineFetcher",
            expectedRespons,
            fetcher.readLine());
      }

      assertNull(
          "LineFetcher had unexpected data after it should have finished", fetcher.readLine());
    }
  }
}
