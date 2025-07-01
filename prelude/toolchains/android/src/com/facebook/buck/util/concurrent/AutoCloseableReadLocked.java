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

import java.util.concurrent.locks.Lock;

/** Locked state of the lock. */
public class AutoCloseableReadLocked extends AutoCloseableLocked {

  private AutoCloseableReadLocked(Lock lock) {
    super(lock);
  }

  /** Lock the lock and return an object which unlocks on close. */
  static AutoCloseableReadLocked createFor(Lock lock) {
    return new AutoCloseableReadLocked(lock);
  }
}
