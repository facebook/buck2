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

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.util.concurrent.TimeUnit;
import org.junit.Test;

public class IncrementingFakeClockTest {
  @Test
  public void millisAddsIncrementAfterCall() {
    Clock clock = new IncrementingFakeClock(TimeUnit.MILLISECONDS.toNanos(64738));
    assertThat(clock.currentTimeMillis(), is(64738L));
  }

  @Test
  public void nanosAddsIncrementAfterCall() {
    Clock clock = new IncrementingFakeClock(49152);
    assertThat(clock.nanoTime(), is(49152L));
  }
}
