/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.kotlin.abtesting.qe2

import com.facebook.buck.core.util.log.Logger
import com.facebook.tools.qe2.QE2Logger

internal object BuckQE2Logger : QE2Logger {

  private val LOG: Logger = Logger.get(BuckQE2Logger::class.java)

  override fun debug(message: String) {
    LOG.debug(message)
  }

  override fun info(message: String, throwable: Throwable?) {
    LOG.info(message, throwable)
  }

  override fun warn(message: String, throwable: Throwable?) {
    LOG.warn(message, throwable)
  }

  override fun error(message: String, throwable: Throwable?) {
    LOG.error(message, throwable)
  }
}
