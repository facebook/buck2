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

import com.google.common.base.Preconditions;
import java.util.concurrent.locks.Lock;

/** Locked state of the lock. */
public abstract class AutoCloseableLocked implements AutoCloseable {
  private final Lock lock;
  // Volatile here is just to make infer happy, this
  // class is not thread safe.
  private volatile boolean locked;

  AutoCloseableLocked(Lock lock) {
    lock.lock();
    this.lock = lock;
    this.locked = true;
  }

  /** This is a dummy function to be used to mute unused locked parameter warning. */
  public void markUsed() {}

  @Override
  public void close() {
    Preconditions.checkState(locked);
    lock.unlock();
    locked = false;
  }
}
