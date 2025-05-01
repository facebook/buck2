/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.util;

import static org.junit.Assert.assertEquals;

import com.facebook.buck.jvm.java.fatjar.WindowsCreateProcessEscape;
import org.junit.Test;

public class WindowsCreateProcessEscapeTest {
  @Test
  public void testCases() {
    // An array of of input strings and the expected output.
    String[][] tests = {
      {
        "C:\\Windows\\", "C:\\Windows\\",
      },
      {
        "", "\"\"",
      },
      {
        " ", "\" \"",
      },
      {
        "\t", "\"\t\"",
      },
      {
        "\\", "\\",
      },
      {
        "\\\\", "\\\\",
      },
      {
        " \\", "\" \\\\\"",
      },
      {
        "\t\\", "\"\t\\\\\"",
      },
      {
        "\\\"", "\"\\\\\\\"\"",
      },
      {
        "\\a\\\"", "\"\\a\\\\\\\"\"",
      },
      {
        "\\\"a\\\"", "\"\\\\\\\"a\\\\\\\"\"",
      },
      {
        "\\\"\\\"", "\"\\\\\\\"\\\\\\\"\"",
      },
    };

    for (String[] test : tests) {
      assertEquals(2, test.length);
      assertEquals(test[1], WindowsCreateProcessEscape.quote(test[0]));
    }
  }
}
