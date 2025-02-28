/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.core.build.execution.context;

import com.facebook.buck.core.build.execution.context.actionid.ActionId;
import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.util.immutables.BuckStyleValueWithBuilder;
import com.facebook.buck.util.Ansi;
import com.facebook.buck.util.ClassLoaderCache;
import com.facebook.buck.util.Console;
import com.facebook.buck.util.ProcessExecutor;
import com.facebook.buck.util.Verbosity;
import com.facebook.buck.util.environment.Platform;
import com.facebook.buck.util.timing.Clock;
import com.google.common.collect.ImmutableMap;
import com.google.common.io.Closer;
import java.io.Closeable;
import java.io.IOException;
import java.io.PrintStream;
import java.util.Optional;
import java.util.UUID;
import org.immutables.value.Value;

/** The context exposed for executing {@code IsolatedStep}s */
@BuckStyleValueWithBuilder
public abstract class IsolatedExecutionContext implements Closeable {

  private final UUID buildId = UUID.randomUUID();

  /** Returns an {@link IsolatedExecutionContext}. */
  public static IsolatedExecutionContext of(
      ClassLoaderCache classLoaderCache,
      Console console,
      Platform platform,
      ProcessExecutor processExecutor,
      AbsPath ruleCellRoot,
      ActionId actionId,
      Clock clock) {
    return ImmutableIsolatedExecutionContext.builder()
        .setConsole(console)
        .setPlatform(platform)
        .setProcessExecutor(processExecutor)
        .setRuleCellRoot(ruleCellRoot)
        .setActionId(actionId)
        .setClock(clock)
        .setClassLoaderCache(classLoaderCache.addRef())
        .build();
  }

  /** Returns an {@link IsolatedExecutionContext}. */
  public static IsolatedExecutionContext of(
      Console console,
      Platform platform,
      ProcessExecutor processExecutor,
      AbsPath ruleCellRoot,
      ActionId actionId,
      Clock clock) {
    return ImmutableIsolatedExecutionContext.builder()
        .setConsole(console)
        .setPlatform(platform)
        .setProcessExecutor(processExecutor)
        .setRuleCellRoot(ruleCellRoot)
        .setActionId(actionId)
        .setClock(clock)
        .build();
  }

  public abstract Console getConsole();

  public abstract Platform getPlatform();

  public abstract ImmutableMap<String, String> getEnvironment();

  public abstract ProcessExecutor getProcessExecutor();

  /**
   * The path to the root cell associated with the current rules being interacted with by this
   * context.
   *
   * <p>For example, consider two cells: cell1 and cell2. If a build like "buck build cell2//:bar"
   * was invoked from cell1, this method would return cell2's path in the context for bar. Note that
   * if anything from cell1 is executed during "buck build cell2//:bar", this method would return
   * cell1's path for those things.
   *
   * <p>The return value of this method does not have anything to do with what command line args
   * were passed. Consider cell1 and cell2 again. When executing a rule in cell1 (like cell1//:foo),
   * this would return cell1's path. When executing a rule in cell2 (like cell2//:bar) this would
   * return cell2's path.
   */
  public abstract AbsPath getRuleCellRoot();

  /**
   * Returns an id of executing action. Typically represents that fully qualified name of the build
   * target.
   */
  public abstract ActionId getActionId();

  /** Returns clock associated with the current invocation. */
  public abstract Clock getClock();

  @Value.Default
  public ClassLoaderCache getClassLoaderCache() {
    return new ClassLoaderCache();
  }

  @Value.Derived
  public Verbosity getVerbosity() {
    return getConsole().getVerbosity();
  }

  @Value.Derived
  public PrintStream getStdErr() {
    return getConsole().getStdErr();
  }

  @Value.Derived
  public PrintStream getStdOut() {
    return getConsole().getStdErr();
  }

  public UUID getBuildId() {
    return buildId;
  }

  @Value.Derived
  public Ansi getAnsi() {
    return getConsole().getAnsi();
  }

  @Override
  public void close() throws IOException {
    // Using a Closer makes it easy to ensure that exceptions from one of the closeables don't
    // cancel the others.
    try (Closer closer = Closer.create()) {
      registerCloseables(closer);
    }
  }

  protected void registerCloseables(Closer closer) {
    closer.register(getClassLoaderCache()::close);
  }

  /** Creates SubContext */
  public IsolatedExecutionContext createSubContext(
      PrintStream newStdout, PrintStream newStderr, Optional<Verbosity> verbosityOverride) {
    Console console = getConsole();
    Console newConsole =
        new Console(
            verbosityOverride.orElse(console.getVerbosity()),
            newStdout,
            newStderr,
            console.getAnsi());

    // This should replace (or otherwise retain) all of the closeable parts of the context.
    return ImmutableIsolatedExecutionContext.builder()
        .setConsole(newConsole)
        .setPlatform(getPlatform())
        .setProcessExecutor(getProcessExecutor().cloneWithOutputStreams(newStdout, newStderr))
        .setRuleCellRoot(getRuleCellRoot())
        .setClassLoaderCache(getClassLoaderCache().addRef())
        .setEnvironment(getEnvironment())
        .setActionId(getActionId())
        .setClock(getClock())
        .build();
  }

  public static Builder builder() {
    return new Builder();
  }

  @org.immutables.builder.Builder.AccessibleFields
  public static class Builder extends ImmutableIsolatedExecutionContext.Builder {}
}
