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

import java.io.IOException;
import java.io.PrintStream;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.function.Function;

public class FakeProcessExecutor extends DefaultProcessExecutor {

  private final Function<? super ProcessExecutorParams, FakeProcess> processFunction;
  private final Set<ProcessExecutorParams> launchedProcesses;

  public FakeProcessExecutor() {
    this(
        params -> {
          throw new IllegalArgumentException();
        },
        new Console(Verbosity.ALL, System.out, System.err));
  }

  public FakeProcessExecutor(
      Function<? super ProcessExecutorParams, FakeProcess> processFunction, Console console) {
    this(processFunction, console.getStdOut(), console.getStdErr(), console.getVerbosity());
  }

  public FakeProcessExecutor(
      Function<? super ProcessExecutorParams, FakeProcess> processFunction,
      PrintStream stdOutStream,
      PrintStream stdErrStream,
      Verbosity verbosity) {
    super(stdOutStream, stdErrStream, verbosity, ProcessHelper.getInstance());
    this.processFunction = processFunction;
    this.launchedProcesses = new HashSet<>();
  }

  @Override
  public Result launchAndExecute(ProcessExecutorParams params)
      throws IOException, InterruptedException {
    FakeProcess fakeProcess;
    try {
      fakeProcess = processFunction.apply(params);
    } catch (IllegalArgumentException e) {
      throw new IOException(
          String.format(
              "FakeProcessExecutor not configured to run process with params %s", params));
    }
    launchedProcesses.add(params);
    try (LaunchedProcess launchedProcess =
        new LaunchedProcess(fakeProcess, Collections.emptyList())) {
      return getExecutionResult(launchedProcess);
    }
  }

  public boolean isProcessLaunched(ProcessExecutorParams params) {
    return launchedProcesses.contains(params);
  }

  @Override
  public ProcessExecutor cloneWithOutputStreams(
      PrintStream newStdOutStream, PrintStream newStdErrStream) {
    return new FakeProcessExecutor(
        processFunction, newStdOutStream, newStdErrStream, Verbosity.STANDARD_INFORMATION);
  }
}
