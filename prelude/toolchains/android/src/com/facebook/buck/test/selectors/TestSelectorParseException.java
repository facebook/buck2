/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.test.selectors;

/**
 * Errors specific to parsing test selectors.
 *
 * <p>While this could reasonably be a subclass of {@link
 * com.facebook.buck.core.exceptions.HumanReadableException} our desire to keep the dependencies of
 * this package to a minimum means we'll subclass {@link RuntimeException} instead and convert to a
 * {@link com.facebook.buck.core.exceptions.HumanReadableException} elsewhere.
 */
public class TestSelectorParseException extends RuntimeException {
  public TestSelectorParseException(String message) {
    super(message);
  }

  public TestSelectorParseException(String message, Exception cause) {
    super(message, cause);
  }
}
