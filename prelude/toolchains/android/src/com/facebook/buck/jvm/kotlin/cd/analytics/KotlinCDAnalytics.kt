/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.kotlin.cd.analytics

/**
 * An utility class designed for logging operations in KotlinCD.
 *
 * @see KotlinCDLoggingContext for details on parameter handling.
 */
abstract class KotlinCDAnalytics {

  /**
   * Writes the accumulated data in the provided [KotlinCDLoggingContext] to the underlying logging
   * system. This method must be implemented by concrete subclasses to handle the actual logging
   * process.
   */
  abstract fun log(context: KotlinCDLoggingContext)
}
