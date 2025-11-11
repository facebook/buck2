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
import static org.junit.Assert.assertFalse;

import com.facebook.buck.util.environment.Platform;
import java.io.IOException;
import java.util.StringTokenizer;
import org.junit.Test;

public class ProcessExecutorTest {

  @Test
  public void testDontExpectStdout() throws IOException, InterruptedException {
    CapturingPrintStream stdOut = new CapturingPrintStream();
    CapturingPrintStream stdErr = new CapturingPrintStream();
    Ansi ansi = Ansi.forceTty();
    Console console = new Console(Verbosity.ALL, stdOut, stdErr, ansi);
    ProcessExecutor executor = new DefaultProcessExecutor(console);
    String cmd = Platform.detect() == Platform.WINDOWS ? "cmd /C echo Hello" : "echo Hello";
    ProcessExecutorParams params = ProcessExecutorParams.ofCommand(makeCommandArray(cmd));
    ProcessExecutor.Result result = executor.launchAndExecute(params);
    assertEquals(
        ansi.asHighlightedFailureText("Hello" + System.lineSeparator()), result.getStdout().get());
    assertEquals("", result.getStderr().get());
  }

  @Test
  public void testProcessFailureDoesNotWriteEmptyString() throws IOException, InterruptedException {
    String cmd = Platform.detect() == Platform.WINDOWS ? "cmd /C (exit 1)" : "false";
    DirtyPrintStreamDecorator stdOut = new DirtyPrintStreamDecorator(new CapturingPrintStream());
    DirtyPrintStreamDecorator stdErr = new DirtyPrintStreamDecorator(new CapturingPrintStream());
    Ansi ansi = Ansi.forceTty();
    Console console = new Console(Verbosity.ALL, stdOut, stdErr, ansi);
    ProcessExecutor executor = new DefaultProcessExecutor(console);
    ProcessExecutorParams params = ProcessExecutorParams.ofCommand(makeCommandArray(cmd));
    executor.launchAndExecute(params);
    assertFalse(stdOut.isDirty());
    assertFalse(stdErr.isDirty());
  }

  private static String[] makeCommandArray(String command) {
    StringTokenizer st = new StringTokenizer(command);
    String[] cmdArray = new String[st.countTokens()];
    for (int i = 0; st.hasMoreTokens(); i++) {
      cmdArray[i] = st.nextToken();
    }
    return cmdArray;
  }
}
