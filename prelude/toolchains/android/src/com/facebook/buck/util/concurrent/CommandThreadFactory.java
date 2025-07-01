/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.util.concurrent;

import java.util.OptionalInt;
import java.util.concurrent.ThreadFactory;
import javax.annotation.Nullable;

/**
 * A ThreadFactory which associates created threads with the same command associated with the thread
 * which creates the CommandThreadFactory.
 */
public class CommandThreadFactory implements ThreadFactory {

  private final ThreadFactory threadFactory;
  private final CommonThreadFactoryState state;
  @Nullable private final String commandId;
  private final OptionalInt optionalPriority;

  public CommandThreadFactory(
      String threadName, CommonThreadFactoryState state, int threadPriority) {
    this(new MostExecutors.NamedThreadFactory(threadName), state, OptionalInt.of(threadPriority));
  }

  public CommandThreadFactory(String threadName, CommonThreadFactoryState state) {
    this(new MostExecutors.NamedThreadFactory(threadName), state, OptionalInt.empty());
  }

  public CommandThreadFactory(ThreadFactory threadFactory, CommonThreadFactoryState state) {
    this(threadFactory, state, OptionalInt.empty());
  }

  public CommandThreadFactory(
      ThreadFactory threadFactory, CommonThreadFactoryState state, OptionalInt optionalPriority) {
    this.threadFactory = threadFactory;
    this.state = state;

    // This might be null in test environments which bypass `Main.runMainThenExit`.
    this.commandId = state.threadIdToCommandId(Thread.currentThread().getId());
    this.optionalPriority = optionalPriority;
  }

  @Override
  public Thread newThread(Runnable r) {
    Thread newThread = threadFactory.newThread(r);
    if (optionalPriority.isPresent()) {
      newThread.setPriority(optionalPriority.getAsInt());
    }

    if (commandId != null) {
      state.register(newThread.getId(), commandId);
    }

    return newThread;
  }
}
