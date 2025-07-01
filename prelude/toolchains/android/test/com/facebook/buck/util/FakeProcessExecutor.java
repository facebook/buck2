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

import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableMap;
import java.io.IOException;
import java.io.PrintStream;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;

public class FakeProcessExecutor extends DefaultProcessExecutor {

  private final Function<? super ProcessExecutorParams, FakeProcess> processFunction;
  private final Set<ProcessExecutorParams> launchedProcesses;

  public FakeProcessExecutor() {
    this(ImmutableMap.of());
  }

  public FakeProcessExecutor(Map<ProcessExecutorParams, FakeProcess> processMap) {
    this(processMap, new Console(Verbosity.ALL, System.out, System.err, Ansi.withoutTty()));
  }

  public FakeProcessExecutor(
      Iterable<Map.Entry<ProcessExecutorParams, FakeProcess>> processIterable) {
    this(processIterable, new Console(Verbosity.ALL, System.out, System.err, Ansi.withoutTty()));
  }

  public FakeProcessExecutor(
      Iterable<Map.Entry<ProcessExecutorParams, FakeProcess>> processIterable, Console console) {
    this(
        new Function<ProcessExecutorParams, FakeProcess>() {
          final Iterator<Map.Entry<ProcessExecutorParams, FakeProcess>> processIterator =
              processIterable.iterator();

          @Override
          public FakeProcess apply(ProcessExecutorParams params) {
            Preconditions.checkState(
                processIterator.hasNext(),
                "Ran out of fake processes when asked to run %s",
                params);
            Map.Entry<ProcessExecutorParams, FakeProcess> nextProcess = processIterator.next();
            Preconditions.checkState(
                nextProcess.getKey().equals(params),
                "Mismatch when asked to run process %s (expecting %s)",
                params,
                nextProcess.getKey());
            return nextProcess.getValue();
          }
        },
        console);
  }

  public FakeProcessExecutor(Map<ProcessExecutorParams, FakeProcess> processMap, Console console) {
    this(processMap::get, console);
  }

  public FakeProcessExecutor(
      Function<? super ProcessExecutorParams, FakeProcess> processFunction, Console console) {
    this(
        processFunction,
        console.getStdOut(),
        console.getStdErr(),
        console.getAnsi(),
        console.getVerbosity());
  }

  public FakeProcessExecutor(
      Function<? super ProcessExecutorParams, FakeProcess> processFunction,
      PrintStream stdOutStream,
      PrintStream stdErrStream,
      Ansi ansi,
      Verbosity verbosity) {
    super(stdOutStream, stdErrStream, ansi, verbosity, ProcessHelper.getInstance());
    this.processFunction = processFunction;
    this.launchedProcesses = new HashSet<>();
  }

  @Override
  public LaunchedProcess launchProcess(ProcessExecutorParams params) throws IOException {
    try {
      FakeProcess fakeProcess = processFunction.apply(params);
      launchedProcesses.add(params);
      return new LaunchedProcessImpl(fakeProcess, Collections.emptyList());
    } catch (IllegalArgumentException e) {
      throw new IOException(
          String.format(
              "FakeProcessExecutor not configured to run process with params %s", params));
    }
  }

  @Override
  public LaunchedProcess launchProcess(
      ProcessExecutorParams params, ImmutableMap<String, String> context) throws IOException {
    return launchProcess(params);
  }

  public boolean isProcessLaunched(ProcessExecutorParams params) {
    return launchedProcesses.contains(params);
  }

  @Override
  public ProcessExecutor cloneWithOutputStreams(
      PrintStream newStdOutStream, PrintStream newStdErrStream) {
    return new FakeProcessExecutor(
        processFunction,
        newStdOutStream,
        newStdErrStream,
        Ansi.withoutTty(),
        Verbosity.STANDARD_INFORMATION);
  }
}
