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

import java.util.concurrent.locks.ReentrantReadWriteLock;

/**
 * Convenience wrapper around {@link java.util.concurrent.locks.ReentrantReadWriteLock} that, when
 * combined with try-with-resources pattern, automatically unlocks the lock once the {@code try}
 * section is completed.
 *
 * <p>Usage example:
 *
 * <pre>
 *   AutoCloseableReadWriteLock lock = new AutoCloseableReadWriteLock();
 *   try (AutoCloseableLock readLock = lock.readLock()) {
 *     // no other thread can acquire write lock while this section is running
 *   }
 *   // the readLock is automatically unlocked
 * </pre>
 *
 * </pre>
 */
public class AutoCloseableReadWriteLock {

  private final ReentrantReadWriteLock reentrantReadWriteLock;

  public AutoCloseableReadWriteLock() {
    reentrantReadWriteLock = new ReentrantReadWriteLock();
  }

  public AutoCloseableReadLocked lockRead() {
    return AutoCloseableReadLocked.createFor(reentrantReadWriteLock.readLock());
  }

  public AutoCloseableWriteLocked lockWrite() {
    return AutoCloseableWriteLocked.createFor(reentrantReadWriteLock.writeLock());
  }
}
