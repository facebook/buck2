/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.kotlin.cd.analytics

/** No-op implementation of [KotlinCDAnalytics] */
object KotlinCDNoopAnalytics : KotlinCDAnalytics() {
  override fun log(context: KotlinCDLoggingContext): Unit = Unit
}
