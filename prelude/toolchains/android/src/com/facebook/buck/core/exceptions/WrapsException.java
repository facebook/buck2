/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.core.exceptions;

/**
 * This interface indicates that an exception is just used to wrap another and isn't the true root
 * cause of the problem. This is used to present the exception in a more user-friendly way.
 */
public interface WrapsException {

  /**
   * Unwraps exception to the root cause.
   *
   * <p>TODO(cjhopman): This behavior is slightly different than that in ErrorLogger as it doesn't
   * unwrap the non-WrapsException exceptions that ErrorLogger does.
   */
  static Throwable getRootCause(Throwable throwable) {
    Throwable cause = throwable;
    while (cause instanceof WrapsException) {
      cause = cause.getCause();
    }
    return cause;
  }
}
