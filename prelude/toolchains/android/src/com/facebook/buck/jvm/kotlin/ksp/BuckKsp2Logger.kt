/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.kotlin.ksp

import com.facebook.buck.core.util.log.Logger
import com.google.devtools.ksp.processing.KSPLogger
import com.google.devtools.ksp.symbol.FileLocation
import com.google.devtools.ksp.symbol.KSNode
import com.google.devtools.ksp.symbol.NonExistLocation
import java.io.PrintStream
import java.util.logging.Level

/**
 * A logger for KSP that logs to Buck's logging system. Similar to [KotlincStep]'s
 * [BuckKotlinLogger], and KSP1's [MessageCollectorBasedKSPLogger]
 */
class BuckKsp2Logger(private val stdErr: PrintStream) : KSPLogger {

  override fun logging(message: String, symbol: KSNode?) {
    if (!LOG.isDebugEnabled) return
    LOG.debug(convertMessage(message, symbol))
  }

  override fun info(message: String, symbol: KSNode?) {
    if (!LOG.isLoggable(Level.INFO)) return
    LOG.info(convertMessage(message, symbol))
  }

  override fun warn(message: String, symbol: KSNode?) {
    if (!LOG.isLoggable(Level.WARNING)) return
    stdErr.println(convertMessage(message, symbol))
  }

  override fun error(message: String, symbol: KSNode?) {
    if (!LOG.isLoggable(Level.SEVERE)) return
    stdErr.println(convertMessage(message, symbol))
  }

  override fun exception(e: Throwable) {
    if (!LOG.isLoggable(Level.SEVERE)) return
    stdErr.println("$LOGGING_PREFIX${e.stackTraceToString()}")
  }

  private fun convertMessage(message: String, symbol: KSNode?): String =
      when (val location = symbol?.location) {
        is FileLocation -> "$LOGGING_PREFIX${location.filePath}:${location.lineNumber}: $message"
        is NonExistLocation,
        null -> "$LOGGING_PREFIX$message"
      }

  companion object {
    private val LOG: Logger = Logger.get(BuckKsp2Logger::class.java)
    private const val LOGGING_PREFIX: String = "[ksp2] "
  }
}
