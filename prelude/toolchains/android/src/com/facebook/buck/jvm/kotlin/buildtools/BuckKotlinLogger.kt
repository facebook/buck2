/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.kotlin.buildtools

import com.facebook.buck.core.util.log.Logger
import com.facebook.buck.jvm.kotlin.cd.analytics.KotlinCDLoggingContext
import java.io.PrintStream
import java.util.logging.Level
import org.jetbrains.kotlin.buildtools.api.KotlinLogger

internal class BuckKotlinLogger(
    private val stdErr: PrintStream,
    private val loggingContext: KotlinCDLoggingContext
) : KotlinLogger {

  override val isDebugEnabled: Boolean
    get() = LOG.isDebugEnabled

  override fun debug(msg: String) {
    if (!LOG.isDebugEnabled) return
    LOG.debug(msg)
  }

  override fun error(msg: String, throwable: Throwable?) {
    if (!LOG.isLoggable(Level.SEVERE)) return
    stdErr.println(msg)
    throwable?.printStackTrace(stdErr)
  }

  override fun info(msg: String) {
    if (msg.startsWith("Non-incremental compilation will be performed")) {
      loggingContext.addExtras(BuckKotlinLogger::class.java.simpleName, msg)
    }

    if (!LOG.isLoggable(Level.INFO)) return
    LOG.info(msg)
  }

  override fun lifecycle(msg: String) {
    if (!LOG.isLoggable(Level.INFO)) return
    LOG.info(msg)
  }

  override fun warn(msg: String) {
    if (!LOG.isLoggable(Level.WARNING)) return
    stdErr.println(msg)
  }

  companion object {
    private val LOG: Logger = Logger.get(BuckKotlinLogger::class.java)
  }
}
