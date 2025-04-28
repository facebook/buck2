/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.testutil;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

/** Unit test for {@link TestConsole}. */
public class TestConsoleTest {

  @Test
  public void dontCrossTheStreams() {
    TestConsole console = new TestConsole();
    console.getStdOut().println("foo");
    console.getStdErr().println("bar");

    assertEquals("foo" + System.lineSeparator(), console.getTextWrittenToStdOut());
    assertEquals("bar" + System.lineSeparator(), console.getTextWrittenToStdErr());
  }
}
