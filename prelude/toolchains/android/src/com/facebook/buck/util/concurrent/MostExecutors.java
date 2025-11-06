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

import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;

public class MostExecutors {

  private MostExecutors() {
    // Utility class.
  }

  /**
   * Cancel the processing being carried out by the given service and waits for the processing to
   * complete. If processing has still not terminated the method throws the given exception.
   */
  public static void shutdownOrThrow(
      ExecutorService service, long timeout, TimeUnit unit, RuntimeException exception) {
    boolean terminated = false;
    service.shutdown();
    try {
      terminated = service.awaitTermination(timeout, unit);
    } catch (InterruptedException e) {
      terminated = false;
    } finally {
      if (!terminated) {
        service.shutdownNow();
        try {
          terminated = service.awaitTermination(timeout, unit);
        } catch (InterruptedException e) {
          terminated = false;
        }
      }
    }
    if (!terminated) {
      throw exception;
    }
  }
}
