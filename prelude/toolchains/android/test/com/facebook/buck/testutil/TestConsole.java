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

import com.facebook.buck.util.Ansi;
import com.facebook.buck.util.CapturingPrintStream;
import com.facebook.buck.util.Console;
import com.facebook.buck.util.Verbosity;
import java.nio.charset.StandardCharsets;

/**
 * Implementation of {@link Console} to use in unit tests. Avoids the side-effect of writing to
 * actual {@link System#out} or {@link System#err}, and makes it possible to inspect what was
 * written to either stream after the fact.
 */
public class TestConsole extends Console {

  public TestConsole() {
    this(Verbosity.STANDARD_INFORMATION);
  }

  public TestConsole(Verbosity verbosity) {
    super(
        verbosity,
        /* stdOut */ new CapturingPrintStream(),
        /* stdErr */ new CapturingPrintStream(),
        /* ansi */ Ansi.withoutTty());
  }

  public String getTextWrittenToStdOut() {
    CapturingPrintStream stream = (CapturingPrintStream) getStdOut().getRawStream();
    return stream.getContentsAsString(StandardCharsets.UTF_8);
  }

  public String getTextWrittenToStdErr() {
    CapturingPrintStream stream = (CapturingPrintStream) getStdErr().getRawStream();
    return stream.getContentsAsString(StandardCharsets.UTF_8);
  }
}
