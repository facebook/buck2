/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.util;

import com.facebook.buck.core.util.log.Logger;
import com.google.common.base.Throwables;

public class Threads {

  private static final Logger LOG = Logger.get(Threads.class);

  /** Utility class: do not instantiate. */
  private Threads() {}

  public static Thread namedThread(String name, Runnable runnable) {
    Thread newThread = new Thread(runnable);
    newThread.setName(name);
    return newThread;
  }

  public static void interruptCurrentThread() {
    LOG.warn(
        "Current thread interrupted at this location: "
            + Throwables.getStackTraceAsString(new Throwable()));
    Thread.currentThread().interrupt();
  }

  public static void interruptThread(Thread thread) {
    LOG.warn(
        "Thread interrupted at this location: "
            + Throwables.getStackTraceAsString(new Throwable()));
    thread.interrupt();
  }
}
