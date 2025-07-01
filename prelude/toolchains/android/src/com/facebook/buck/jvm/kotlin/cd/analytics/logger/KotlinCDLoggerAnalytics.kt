/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.kotlin.cd.analytics.logger

import com.facebook.buck.core.util.log.Logger
import com.facebook.buck.jvm.cd.command.kotlin.LanguageVersionForLogs
import com.facebook.buck.jvm.kotlin.cd.analytics.KotlinCDAnalytics
import com.facebook.buck.jvm.kotlin.cd.analytics.KotlinCDLoggingContext
import com.facebook.buck.jvm.kotlin.cd.analytics.KotlincModeParam
import com.facebook.buck.jvm.kotlin.cd.analytics.logger.model.KotlinCDLogEntry
import java.time.Clock
import java.time.Duration
import java.time.Instant

class KotlinCDLoggerAnalytics
@JvmOverloads
constructor(
    private val kotlinCDLogger: KotlinCDLogger,
    private val buildUuid: String?,
    private val target: String,
    private val subtarget: String,
    private val executionPlatform: String,
    private val numJavaFiles: Long,
    private val numKotlinFiles: Long,
    private val incremental: Boolean,
    private val clock: Clock = Clock.systemDefaultZone()
) : KotlinCDAnalytics() {

  override fun log(context: KotlinCDLoggingContext) {
    if (buildUuid == null) {
      LOG.debug(
          "No operation performed. This is expected when running an action downloaded from RE.")
      return
    }

    val start = Instant.now(clock)
    val success: Boolean = kotlinCDLogger.log(createKotlinCDLogEntry(context))
    val end = Instant.now(clock)

    val duration = Duration.between(start, end)
    if (success) {
      LOG.info(
          "Successfully wrote KotlinCD logs to scribe. Total time: " +
              duration.toMillis() +
              " milliseconds")
    } else {
      LOG.warn(
          (("Failed to write KotlinCD logs to scribe. Total time: " +
              duration.toMillis() +
              " milliseconds")))
    }
  }

  @OptIn(LanguageVersionForLogs::class)
  private fun createKotlinCDLogEntry(context: KotlinCDLoggingContext): KotlinCDLogEntry {
    val addedAndModifiedFiles: Set<String>? =
        (context.kotlincMode as? KotlincModeParam.Incremental)
            ?.addedAndModifiedFiles
            ?.map { it.toString() }
            ?.toSet()
    val removedFiles: Set<String>? =
        (context.kotlincMode as? KotlincModeParam.Incremental)
            ?.removedFiles
            ?.map { it.toString() }
            ?.toSet()

    return KotlinCDLogEntry(
        time = Instant.now(clock).epochSecond,
        eventTime = Instant.now(clock).epochSecond.toDouble(),
        target = target,
        subtarget = subtarget,
        buildUuid = requireNotNull(buildUuid) { "buildUuid is required" },
        executionPlatform = executionPlatform,
        numKotlinFiles = numKotlinFiles,
        numJavaFiles = numJavaFiles,
        incremental = incremental,
        kotlincMode = context.kotlincMode?.value,
        classpathChanges = context.classpathChangesParam?.value,
        step = context.step.value,
        languageVersion = context.languageVersion.valueForLogs,
        extras = buildJson(context.extras),
        addedAndModifiedFiles = addedAndModifiedFiles,
        removedFiles = removedFiles,
    )
  }

  private fun buildJson(extras: Map<String, List<String>>): String? {
    if (extras.isEmpty()) {
      return null
    }

    return "{${extras.map { (key, list) -> "\"${key}\": [${list.joinToString(transform = { "\"$it\"" })}]" }.joinToString()}}"
  }

  companion object {
    private val LOG: Logger = Logger.get(KotlinCDLoggerAnalytics::class.java)
  }
}
