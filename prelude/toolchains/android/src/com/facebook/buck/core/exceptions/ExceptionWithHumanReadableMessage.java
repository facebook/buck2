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

import javax.annotation.Nullable;

public interface ExceptionWithHumanReadableMessage {

  /**
   * @return a human-readable error message
   */
  String getHumanReadableErrorMessage();

  /** Get the dependency stack associated with this error */
  default DependencyStack getDependencyStack() {
    return DependencyStack.root();
  }

  /** Get dependency stack if the exception have some otherwise return an empty stack. */
  static DependencyStack getDependencyStack(@Nullable Throwable throwable) {
    if (throwable instanceof ExceptionWithHumanReadableMessage) {
      return ((ExceptionWithHumanReadableMessage) throwable).getDependencyStack();
    } else {
      return DependencyStack.root();
    }
  }
}
