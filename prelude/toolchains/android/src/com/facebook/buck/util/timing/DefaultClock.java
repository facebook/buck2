/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.util.timing;

import java.lang.management.ManagementFactory;
import java.lang.management.ThreadMXBean;

/** {@link Clock} implementation that invokes the {@link System} calls. */
public class DefaultClock implements Clock {

  private final ThreadMXBean threadMXBean;
  private final boolean userNanoTimeEnabled;

  public DefaultClock() {
    this(true);
  }

  public DefaultClock(boolean enableThreadCpuTime) {
    threadMXBean = ManagementFactory.getThreadMXBean();
    userNanoTimeEnabled = enableThreadCpuTime && threadMXBean.isThreadCpuTimeEnabled();
  }

  @Override
  public long currentTimeMillis() {
    return System.currentTimeMillis();
  }

  @Override
  public long nanoTime() {
    return System.nanoTime();
  }

  @Override
  public long threadUserNanoTime(long threadId) {
    if (!userNanoTimeEnabled) {
      return -1;
    }
    return threadMXBean.getThreadUserTime(threadId);
  }
}
