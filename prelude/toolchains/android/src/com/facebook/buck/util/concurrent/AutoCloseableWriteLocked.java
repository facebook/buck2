/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.util.concurrent;

import java.util.concurrent.locks.Lock;

/** Locked state of the lock. */
public class AutoCloseableWriteLocked extends AutoCloseableLocked {

  private AutoCloseableWriteLocked(Lock lock) {
    super(lock);
  }

  /** Lock the lock and return an object which unlocks on close. */
  static AutoCloseableWriteLocked createFor(Lock lock) {
    return new AutoCloseableWriteLocked(lock);
  }
}
