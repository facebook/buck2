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

import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * A ThreadFactory which gives each thread a meaningful and distinct name. ThreadFactoryBuilder is
 * not used to avoid a dependency on Guava in the junit target.
 */
public class NamedThreadFactory implements ThreadFactory {

  private final AtomicInteger threadCount = new AtomicInteger(0);
  private final String threadName;

  public NamedThreadFactory(String threadName) {
    this.threadName = threadName;
  }

  @Override
  public Thread newThread(Runnable r) {
    Thread newThread = Executors.defaultThreadFactory().newThread(r);
    newThread.setName(String.format(threadName + "-%d", threadCount.incrementAndGet()));
    return newThread;
  }
}
