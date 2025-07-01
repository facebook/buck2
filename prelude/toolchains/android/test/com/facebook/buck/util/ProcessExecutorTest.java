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
import static org.junit.Assert.assertTrue;

import com.facebook.buck.testutil.TestConsole;
import com.facebook.buck.util.environment.Platform;
import com.google.common.collect.ImmutableSet;
import java.io.IOException;
import java.util.EnumSet;
import java.util.Optional;
import java.util.StringTokenizer;
import java.util.concurrent.atomic.AtomicBoolean;
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
  public void testExpectStdout() throws IOException, InterruptedException {
    String cmd = Platform.detect() == Platform.WINDOWS ? "cmd /C echo Hello" : "echo Hello";
    CapturingPrintStream stdOut = new CapturingPrintStream();
    CapturingPrintStream stdErr = new CapturingPrintStream();
    Ansi ansi = Ansi.forceTty();
    Console console = new Console(Verbosity.ALL, stdOut, stdErr, ansi);
    ProcessExecutor executor = new DefaultProcessExecutor(console);
    ProcessExecutorParams params = ProcessExecutorParams.ofCommand(makeCommandArray(cmd));
    ProcessExecutor.Result result =
        executor.launchAndExecute(
            params,
            EnumSet.of(ProcessExecutor.Option.EXPECTING_STD_OUT),
            /* stdin */ Optional.empty(),
            /* timeOutMs */ Optional.empty(),
            /* timeOutHandler */ Optional.empty());
    assertEquals("Hello" + System.lineSeparator(), result.getStdout().get());
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

  @Test
  public void testProcessTimeoutHandlerIsInvoked() throws IOException, InterruptedException {
    @SuppressWarnings("PMD.PrematureDeclaration")
    ProcessExecutor executor = new DefaultProcessExecutor(new TestConsole(Verbosity.ALL));

    AtomicBoolean called = new AtomicBoolean(false);
    String cmd = (Platform.detect() == Platform.WINDOWS) ? "ping -n 50 0.0.0.0" : "sleep 50";
    ProcessExecutorParams params = ProcessExecutorParams.ofCommand(makeCommandArray(cmd));
    ProcessExecutor.Result result =
        executor.launchAndExecute(
            params,
            /* options */ ImmutableSet.<ProcessExecutor.Option>builder().build(),
            /* stdin */ Optional.empty(),
            /* timeOutMs */ Optional.of((long) 100),
            /* timeOutHandler */ Optional.of(ignored -> called.set(true)));
    assertTrue("process was reported as timed out", result.isTimedOut());
    assertTrue("timeOutHandler was called when a timeout was hit", called.get());
  }

  @Test
  public void testProcessTimeoutHandlerThrowsException() throws IOException, InterruptedException {
    @SuppressWarnings("PMD.PrematureDeclaration")
    ProcessExecutor executor = new DefaultProcessExecutor(new TestConsole(Verbosity.ALL));

    String cmd = (Platform.detect() == Platform.WINDOWS) ? "ping -n 50 0.0.0.0" : "sleep 50";
    ProcessExecutorParams params = ProcessExecutorParams.ofCommand(makeCommandArray(cmd));
    ProcessExecutor.Result result =
        executor.launchAndExecute(
            params,
            /* options */ ImmutableSet.<ProcessExecutor.Option>builder().build(),
            /* stdin */ Optional.empty(),
            /* timeOutMs */ Optional.of((long) 100),
            /* timeOutHandler */ Optional.of(
                ignored -> {
                  throw new RuntimeException("This shouldn't fail the test!");
                }));
    assertTrue("process was reported as timed out", result.isTimedOut());
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
