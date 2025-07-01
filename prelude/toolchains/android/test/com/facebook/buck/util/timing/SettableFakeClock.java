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
import java.util.concurrent.atomic.AtomicReference;

/** Fake implementation of {@link Clock} which returns the last time to which it was set. */
public class SettableFakeClock implements Clock {
  public static final SettableFakeClock DO_NOT_CARE = new SettableFakeClock(FakeClock.doNotCare());

  // We use FakeClock as the implementation because it enforces reasonably realistic behavior.
  private final AtomicReference<FakeClock> currentClock;

  public SettableFakeClock(FakeClock initial) {
    currentClock = new AtomicReference<>(initial);
  }

  /**
   * {@code currentTimeMillis} and {@code nanoTime} should be completely unrelated, because {@link
   * System#currentTimeMillis()} and {@link System#nanoTime()} are completely unrelated.
   */
  public SettableFakeClock(long currentTimeMillis, long nanoTime) {
    currentClock = new AtomicReference<>(FakeClock.of(currentTimeMillis, nanoTime));
  }

  public void setCurrentTimeMillis(long millis) {
    currentClock.set(currentClock.get().withCurrentTimeMillis(millis));
  }

  public void advanceTimeNanos(long nanos) {
    FakeClock currentClock = this.currentClock.get();
    // TODO(jkeljo): The clocks do not necessarily advance in lockstep; change this to be more
    // realistic.
    this.currentClock.set(
        currentClock
            .withNanoTime(currentClock.nanoTime() + nanos)
            .withCurrentTimeMillis(
                currentClock.currentTimeMillis() + TimeUnit.NANOSECONDS.toMillis(nanos)));
  }

  @Override
  public long currentTimeMillis() {
    return currentClock.get().currentTimeMillis();
  }

  @Override
  public long nanoTime() {
    return currentClock.get().nanoTime();
  }

  @Override
  public long threadUserNanoTime(long threadId) {
    return currentClock.get().threadUserNanoTime(threadId);
  }
}
