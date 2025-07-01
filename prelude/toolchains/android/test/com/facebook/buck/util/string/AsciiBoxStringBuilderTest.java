/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.util.string;

import static org.junit.Assert.assertEquals;

import com.google.common.base.Joiner;
import org.junit.Test;

public class AsciiBoxStringBuilderTest {
  private static final int MAX_LENGTH = 20;

  private AsciiBoxStringBuilder builder = new AsciiBoxStringBuilder(MAX_LENGTH);

  @Test
  public void testPutsBoxAroundLine() {
    assertEquals(
        Joiner.on('\n')
            .join(
                "+----------------------+",
                "|                      |",
                "| Hello                |",
                "|                      |",
                "+----------------------+",
                ""),
        builder.writeLine("Hello").toString());
  }

  @Test
  public void testWrapsTooLongLines() {
    assertEquals(
        Joiner.on('\n')
            .join(
                "+----------------------+",
                "|                      |",
                "| Hello world, how     |",
                "| are you doing?       |",
                "|                      |",
                "+----------------------+",
                ""),
        builder.writeLine("Hello world, how are you doing?").toString());
  }

  @Test
  public void testWrapsJustBarelyTooLongLines() {
    assertEquals(
        Joiner.on('\n')
            .join(
                "+----------------------+",
                "|                      |",
                "| Hello world, how     |",
                "| you?                 |",
                "|                      |",
                "+----------------------+",
                ""),
        builder.writeLine("Hello world, how you?").toString());
  }

  @Test
  public void testHandlesZeroLength() {
    builder = new AsciiBoxStringBuilder(0);
    assertEquals(
        Joiner.on('\n')
            .join(
                "+---+", "|   |", "| H |", "| e |", "| l |", "| l |", "| o |", "|   |", "+---+",
                ""),
        builder.writeLine("Hello").toString());
  }

  @Test
  public void testSplitsWordsThatAreTooLong() {
    builder = new AsciiBoxStringBuilder(1);
    assertEquals(
        Joiner.on('\n')
            .join(
                "+---+", "|   |", "| H |", "| e |", "| l |", "| l |", "| o |", "|   |", "| w |",
                "| o |", "| r |", "| l |", "| d |", "|   |", "+---+", ""),
        builder.writeLine("Hello world").toString());
  }
}
