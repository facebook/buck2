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

import com.facebook.buck.core.util.immutables.BuckStyleValue;
import com.google.common.base.Preconditions;
import java.util.concurrent.TimeUnit;
import org.immutables.value.Value;

/** Provides a fake implementation of a {@link Clock} which always returns a constant time. */
@BuckStyleValue
public abstract class FakeClock implements Clock {
  /** FakeClock instance for tests that don't require specific timestamp values. */
  public static FakeClock doNotCare() {
    return of(1337, TimeUnit.MILLISECONDS.toNanos(4242));
  }

  @Override
  public abstract long currentTimeMillis();

  @Override
  public abstract long nanoTime();

  @Value.Check
  protected void checkNanoTimeIsNotDerivedFromCurrentTimeMillis() {
    // Being a little overly conservative here given the method name, but really nano time should
    // never be anywhere near currentTimeMillis so it's OK.
    Preconditions.checkState(
        Math.abs(TimeUnit.NANOSECONDS.toMillis(nanoTime()) - currentTimeMillis()) > 1);
  }

  @Override
  public long threadUserNanoTime(long threadId) {
    return -1;
  }

  public static FakeClock of(long currentTimeMillis, long nanoTime) {
    return ImmutableFakeClock.ofImpl(currentTimeMillis, nanoTime);
  }

  public FakeClock withCurrentTimeMillis(long millis) {
    if (currentTimeMillis() == millis) {
      return this;
    }
    return of(millis, nanoTime());
  }

  public FakeClock withNanoTime(long nanoTime) {
    if (nanoTime() == nanoTime) {
      return this;
    }
    return of(currentTimeMillis(), nanoTime);
  }
}
