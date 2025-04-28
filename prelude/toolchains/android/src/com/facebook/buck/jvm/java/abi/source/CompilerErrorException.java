/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java.abi.source;

/**
 * Thrown when an error in source code is detected in a place that doesn't have enough context to
 * report a graceful compiler error. Caught at a higher level, where the message is reported as a
 * compiler error with relevant context.
 */
class CompilerErrorException extends Exception {
  public CompilerErrorException(String message) {
    super(message);
  }
}
