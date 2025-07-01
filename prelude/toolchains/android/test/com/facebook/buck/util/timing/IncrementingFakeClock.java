/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.util.timing;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Provides a fake implementation of a {@link Clock} which increments both {@link
 * #currentTimeMillis()} and {@link #nanoTime()} by a fixed amount every time either is queried.
 */
public class IncrementingFakeClock implements Clock {
  private final AtomicLong counter;
  private final long increment;

  public IncrementingFakeClock() {
    this(1);
  }

  public IncrementingFakeClock(long increment) {
    this.counter = new AtomicLong();
    this.increment = increment;
  }

  @Override
  public long currentTimeMillis() {
    // In our world, currentTimeMillis() is based on nanoTime().
    // This isn't the case for normal clocks, but it works for us.
    return TimeUnit.NANOSECONDS.toMillis(nanoTime());
  }

  @Override
  public long nanoTime() {
    return counter.addAndGet(increment);
  }

  @Override
  public long threadUserNanoTime(long threadId) {
    return -1L;
  }
}
