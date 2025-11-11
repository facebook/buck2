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

import com.google.common.collect.ImmutableList;
import java.io.IOException;
import java.io.PrintStream;
import java.util.Optional;

public interface ProcessExecutor {

  /** Launches and executes a {@link Process} given {@link ProcessExecutorParams}. */
  Result launchAndExecute(ProcessExecutorParams params) throws InterruptedException, IOException;

  /** Makes a clone of this process executor with the stdout and stderr streams overridden. */
  ProcessExecutor cloneWithOutputStreams(PrintStream stdOutStream, PrintStream stdErrStream);

  /** Values from the result of {@link ProcessExecutor#launchAndExecute(ProcessExecutorParams)}. */
  class Result {

    private final int exitCode;
    private final Optional<String> stdout;
    private final Optional<String> stderr;
    private final ImmutableList<String> command;

    public Result(
        int exitCode,
        Optional<String> stdout,
        Optional<String> stderr,
        ImmutableList<String> command) {
      this.exitCode = exitCode;
      this.stdout = stdout;
      this.stderr = stderr;
      this.command = command;
    }

    public Result(int exitCode, ImmutableList<String> command) {
      this(exitCode, Optional.empty(), Optional.empty(), command);
    }

    public int getExitCode() {
      return exitCode;
    }

    public Optional<String> getStdout() {
      return stdout;
    }

    public Optional<String> getStderr() {
      return stderr;
    }

    public ImmutableList<String> getCommand() {
      return command;
    }
  }
}
