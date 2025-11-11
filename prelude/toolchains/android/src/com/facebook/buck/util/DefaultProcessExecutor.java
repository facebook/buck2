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

import static com.google.common.base.Predicates.not;
import static java.nio.charset.StandardCharsets.UTF_8;

import com.facebook.buck.core.util.log.Logger;
import com.facebook.buck.util.environment.Platform;
import com.google.common.annotations.VisibleForTesting;
import com.google.common.collect.ImmutableList;
import com.google.common.util.concurrent.ThreadFactoryBuilder;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.SynchronousQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/** Executes a {@link Process} and blocks until it is finished. */
public class DefaultProcessExecutor implements ProcessExecutor {

  private static final Logger LOG = Logger.get(ProcessExecutor.class);

  private static final ThreadPoolExecutor PROCESS_EXECUTOR_THREAD_POOL =
      new ThreadPoolExecutor(
          0,
          Integer.MAX_VALUE,
          1,
          TimeUnit.SECONDS,
          new SynchronousQueue<>(),
          new ThreadFactoryBuilder().setNameFormat("ProcessExecutor").build());

  private final PrintStream stdOutStream;
  private final PrintStream stdErrStream;
  private final Ansi ansi;
  private final Verbosity verbosity;
  private final ProcessHelper processHelper;

  /**
   * Creates a new {@link DefaultProcessExecutor} with the specified parameters used for writing the
   * output of the process.
   */
  public DefaultProcessExecutor(Console console) {
    this(
        console.getStdOut(),
        console.getStdErr(),
        console.getAnsi(),
        console.getVerbosity(),
        ProcessHelper.getInstance());
  }

  @VisibleForTesting
  protected DefaultProcessExecutor(
      PrintStream stdOutStream,
      PrintStream stdErrStream,
      Ansi ansi,
      Verbosity verbosity,
      ProcessHelper processHelper) {
    this.stdOutStream = stdOutStream;
    this.stdErrStream = stdErrStream;
    this.ansi = ansi;
    this.verbosity = verbosity;
    this.processHelper = processHelper;
  }

  @Override
  public ProcessExecutor cloneWithOutputStreams(
      PrintStream newStdOutStream, PrintStream newStdErrStream) {
    return new DefaultProcessExecutor(
        newStdOutStream, newStdErrStream, ansi, verbosity, processHelper);
  }

  private LaunchedProcess launchProcess(ProcessExecutorParams params) throws IOException {
    ImmutableList<String> command = params.getCommand();
    /* On Windows, we need to escape the arguments we hand off to `CreateProcess`.  See
     * http://blogs.msdn.com/b/twistylittlepassagesallalike/archive/2011/04/23/everyone-quotes-arguments-the-wrong-way.aspx
     * for more details.
     */
    if (Platform.detect() == Platform.WINDOWS) {
      command =
          command.stream()
              .map(Escaper.CREATE_PROCESS_ESCAPER::apply)
              .collect(ImmutableList.toImmutableList());
    }
    ProcessBuilder pb = new ProcessBuilder(command);
    if (params.getDirectory().isPresent()) {
      pb.directory(params.getDirectory().get().toFile());
    }
    if (params.getEnvironment().isPresent()) {
      pb.environment().clear();
      pb.environment().putAll(params.getEnvironment().get());
    }
    Process process = pb.start();
    return new LaunchedProcess(process, pb.command());
  }

  /** Executes the specified already-launched process. */
  @Override
  public Result launchAndExecute(ProcessExecutorParams params)
      throws InterruptedException, IOException {
    try (LaunchedProcess launchedProcess = launchProcess(params)) {
      return getExecutionResult(launchedProcess);
    }
  }

  protected Result getExecutionResult(LaunchedProcess launchedProcess) throws InterruptedException {
    Process process = launchedProcess.process;
    // Read stdout/stderr asynchronously while running a Process.
    // See http://stackoverflow.com/questions/882772/capturing-stdout-when-calling-runtime-exec
    PrintStream stdOutToWriteTo = new CapturingPrintStream();
    InputStreamConsumer stdOut =
        new InputStreamConsumer(
            process.getInputStream(),
            InputStreamConsumer.createAnsiHighlightingHandler(
                /* flagOutputWrittenToStream */ true, stdOutToWriteTo, ansi));

    PrintStream stdErrToWriteTo = new CapturingPrintStream();
    InputStreamConsumer stdErr =
        new InputStreamConsumer(
            process.getErrorStream(),
            InputStreamConsumer.createAnsiHighlightingHandler(
                /* flagOutputWrittenToStream */ true, stdErrToWriteTo, ansi));

    // Consume the streams so they do not deadlock.
    Future<Void> stdOutTerminationFuture = PROCESS_EXECUTOR_THREAD_POOL.submit(stdOut);
    Future<Void> stdErrTerminationFuture = PROCESS_EXECUTOR_THREAD_POOL.submit(stdErr);

    boolean timedOut;

    // Block until the Process completes.
    try {
      // Wait for the process to complete.  If a timeout was given, we wait up to the timeout
      // for it to finish then force kill it.  If no timeout was given, just wait for it forever.
      process.waitFor();
      if (!processHelper.hasProcessFinished(process)) {
        process.destroyForcibly();
      }

      stdOutTerminationFuture.get();
      stdErrTerminationFuture.get();
    } catch (ExecutionException e) {
      // Buck was killed while waiting for the consumers to finish or while writing stdin
      // to the process. This means either the user killed the process or a step failed
      // causing us to kill all other running steps. Neither of these is an exceptional
      // situation.
      LOG.warn(e, "Process threw exception when being executed.");
      return new Result(1, launchedProcess.getCommand());
    } finally {
      process.destroy();
      process.waitFor();
    }

    Optional<String> stdoutText = Optional.of(getData(stdOutToWriteTo));
    Optional<String> stderrText = Optional.of(getData(stdErrToWriteTo));

    // Report the exit code of the Process.
    int exitCode = process.exitValue();

    // If the command has failed and we're not being explicitly quiet, ensure everything gets
    // printed.
    if (exitCode != 0) {
      stdoutText
          .filter(not(String::isEmpty))
          .ifPresent(
              text -> {
                LOG.verbose("Writing captured stdout text to stream: [%s]", text);
                stdOutStream.print(text);
              });

      stderrText
          .filter(not(String::isEmpty))
          .ifPresent(
              text -> {
                LOG.verbose("Writing captured stderr text to stream: [%s]", text);
                stdErrStream.print(text);
              });
    }

    return new Result(exitCode, stdoutText, stderrText, launchedProcess.getCommand());
  }

  private static String getData(PrintStream printStream) {
    CapturingPrintStream capturingPrintStream = (CapturingPrintStream) printStream;
    return capturingPrintStream.getContentsAsString(UTF_8);
  }

  /**
   * Wraps a {@link Process} and exposes only its I/O streams, so callers have to pass it back to
   * this class.
   */
  @VisibleForTesting
  public static class LaunchedProcess implements AutoCloseable {

    public final Process process;
    public final ImmutableList<String> command;

    public LaunchedProcess(Process process, List<String> command) {
      this.process = process;
      this.command = ImmutableList.copyOf(command);
    }

    public boolean isAlive() {
      return process.isAlive();
    }

    public ImmutableList<String> getCommand() {
      return command;
    }

    public OutputStream getStdin() {
      return process.getOutputStream();
    }

    public InputStream getStdout() {
      return process.getInputStream();
    }

    public InputStream getStderr() {
      return process.getErrorStream();
    }

    @Override
    public void close() {
      process.destroy();
    }
  }
}
