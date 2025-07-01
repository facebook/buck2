/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
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
