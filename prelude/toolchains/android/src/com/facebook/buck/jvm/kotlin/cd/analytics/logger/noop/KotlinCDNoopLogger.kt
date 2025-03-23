/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.kotlin.cd.analytics.logger.noop

import com.facebook.buck.jvm.kotlin.cd.analytics.logger.KotlinCDLogger
import com.facebook.buck.jvm.kotlin.cd.analytics.logger.model.KotlinCDLogEntry

/** No-op implementation of [KotlinCDAnalytics] */
class KotlinCDNoopLogger : KotlinCDLogger {

  override fun log(logEntry: KotlinCDLogEntry): Boolean = false
}
