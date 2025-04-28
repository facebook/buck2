/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.kotlin.cd.analytics.logger

import com.facebook.buck.jvm.kotlin.cd.analytics.logger.model.KotlinCDLogEntry
import java.util.ServiceLoader

interface KotlinCDLogger {
  fun log(logEntry: KotlinCDLogEntry): Boolean

  companion object {
    @JvmStatic
    fun loadImplementation(): KotlinCDLogger {
      val implementations = ServiceLoader.load(KotlinCDLogger::class.java)
      implementations.firstOrNull()
          ?: error(
              "The classpath contains no implementation for ${KotlinCDLogger::class.qualifiedName}")
      return implementations.singleOrNull()
          ?: error(
              "The classpath contains more than one implementation for ${KotlinCDLogger::class.qualifiedName}")
    }
  }
}
