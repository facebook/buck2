/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.kotlin.cd.analytics.logger

import com.facebook.buck.jvm.cd.command.kotlin.LanguageVersion
import com.facebook.buck.jvm.kotlin.cd.analytics.ClasspathChangesParam
import com.facebook.buck.jvm.kotlin.cd.analytics.KotlinCDLoggingContext
import com.facebook.buck.jvm.kotlin.cd.analytics.KotlincModeParam
import com.facebook.buck.jvm.kotlin.cd.analytics.StepParam
import com.facebook.buck.jvm.kotlin.cd.logger.KotlinCDLogger
import com.facebook.buck.jvm.kotlin.cd.logger.model.KotlinCDLogEntry
import java.time.Clock
import java.time.Instant
import java.time.ZoneOffset
import org.junit.Test
import org.mockito.kotlin.any
import org.mockito.kotlin.mock
import org.mockito.kotlin.never
import org.mockito.kotlin.times
import org.mockito.kotlin.verify

internal class KotlinCDLoggerAnalyticsTest {

  private val kotlinCDLogger: KotlinCDLogger = mock()

  private val clock = Clock.fixed(Instant.parse("2018-08-22T10:00:00Z"), ZoneOffset.UTC)

  @Test
  fun `when buildUuid is null, data are not logged`() {
    val kotlinCDAnalytics = createFakeKotlinCDAnalytics(buildUuid = null)
    kotlinCDAnalytics.log(createKotlinCDLoggingContext())

    verify(kotlinCDLogger, never()).log(any())
  }

  @Test
  fun `when buildUuid is not null, data are logged`() {
    val kotlinCDAnalytics = createFakeKotlinCDAnalytics()
    kotlinCDAnalytics.log(createKotlinCDLoggingContext())

    verify(kotlinCDLogger, times(1)).log(any())
  }

  @Test
  fun `when language version has major-minor format, it is logged`() {
    val kotlinCDAnalytics = createFakeKotlinCDAnalytics()
    val expectedEntry = createExpectedKotlinCDLogEntry(languageVersion = "2.0")

    kotlinCDAnalytics.log(createKotlinCDLoggingContext(languageVersion = "2.0"))

    verify(kotlinCDLogger, times(1)).log(expectedEntry)
  }

  @Test
  fun `when language version has major-minor-path format, it is logged`() {
    val kotlinCDAnalytics = createFakeKotlinCDAnalytics()
    val expectedEntry = createExpectedKotlinCDLogEntry(languageVersion = "2.0.0")

    kotlinCDAnalytics.log(createKotlinCDLoggingContext(languageVersion = "2.0.0"))

    verify(kotlinCDLogger, times(1)).log(expectedEntry)
  }

  @Test(expected = IllegalArgumentException::class)
  fun `when language version is not valid, exception is thrown`() {
    val kotlinCDAnalytics = createFakeKotlinCDAnalytics()

    kotlinCDAnalytics.log(createKotlinCDLoggingContext(languageVersion = "languageVersion"))
  }

  @Test
  fun `when extras contains a single key-value pair, correct json data are logged`() {
    val kotlinCDAnalytics = createFakeKotlinCDAnalytics()
    val expectedEntry = createExpectedKotlinCDLogEntry(extras = """{"testKey": ["testValue"]}""")

    kotlinCDAnalytics.log(
        createKotlinCDLoggingContext(extras = mapOf("testKey" to listOf("testValue"))))

    verify(kotlinCDLogger, times(1)).log(expectedEntry)
  }

  @Test
  fun `when extras contains a single key-list pair, correct json data are logged`() {
    val kotlinCDAnalytics = createFakeKotlinCDAnalytics()
    val expectedEntry =
        createExpectedKotlinCDLogEntry(extras = """{"testKey": ["testValue1", "testValue2"]}""")

    kotlinCDAnalytics.log(
        createKotlinCDLoggingContext(
            extras = mapOf("testKey" to listOf("testValue1", "testValue2"))))

    verify(kotlinCDLogger, times(1)).log(expectedEntry)
  }

  @Test
  fun `when extras contains a list of key-list pairs, correct json data are logged`() {
    val kotlinCDAnalytics = createFakeKotlinCDAnalytics()
    val expectedEntry =
        createExpectedKotlinCDLogEntry(
            extras =
                """{"testKey1": ["testValue1", "testValue2"], "testKey2": ["testValue3", "testValue4"]}""")

    kotlinCDAnalytics.log(
        createKotlinCDLoggingContext(
            extras =
                mapOf(
                    "testKey1" to listOf("testValue1", "testValue2"),
                    "testKey2" to listOf("testValue3", "testValue4"))))

    verify(kotlinCDLogger, times(1)).log(expectedEntry)
  }

  private fun createKotlinCDLoggingContext(
      step: StepParam = StepParam.KOTLINC,
      languageVersion: String = DEFAULT_LANGUAGE_VERSION,
      kotlincMode: KotlincModeParam? =
          KotlincModeParam.Incremental(ClasspathChangesParam.NO_CHANGES),
      extras: Map<String, List<String>> = mapOf()
  ): KotlinCDLoggingContext {
    val context = KotlinCDLoggingContext(step, LanguageVersion(languageVersion), kotlincMode)
    extras.forEach { (key, extras) -> extras.forEach { item -> context.addExtras(key, item) } }
    return context
  }

  private fun createFakeKotlinCDAnalytics(buildUuid: String? = DEFAULT_BUILDUUID) =
      KotlinCDLoggerAnalytics(
          kotlinCDLogger = kotlinCDLogger,
          buildUuid = buildUuid,
          target = TARGET,
          subtarget = SUBTARGET,
          executionPlatform = EXECTION_PLATFORM,
          numJavaFiles = NUM_JAVA_FILES,
          numKotlinFiles = NUM_KOTLIN_FILES,
          incremental = INCREMENTAL,
          clock = clock)

  private fun createExpectedKotlinCDLogEntry(
      step: StepParam = StepParam.KOTLINC,
      languageVersion: String? = DEFAULT_LANGUAGE_VERSION,
      kotlincMode: KotlincModeParam? =
          KotlincModeParam.Incremental(ClasspathChangesParam.NO_CHANGES),
      extras: String? = null
  ) =
      KotlinCDLogEntry(
          time = Instant.now(clock).epochSecond,
          eventTime = Instant.now(clock).epochSecond.toDouble(),
          target = TARGET,
          subtarget = SUBTARGET,
          buildUuid = BUILD_UUID,
          executionPlatform = EXECTION_PLATFORM,
          numKotlinFiles = NUM_KOTLIN_FILES,
          numJavaFiles = NUM_JAVA_FILES,
          incremental = INCREMENTAL,
          kotlincMode = kotlincMode?.value,
          classpathChanges =
              (kotlincMode as? KotlincModeParam.Incremental)?.classpathChangesParam?.value,
          step = step.value,
          languageVersion = languageVersion,
          extras = extras)

  companion object TestParams {
    private const val TARGET = "target"
    private const val SUBTARGET = "subtarget"
    private const val BUILD_UUID = "buildUuid"
    private const val EXECTION_PLATFORM = "executionPlatform"
    private const val NUM_KOTLIN_FILES = 5L
    private const val NUM_JAVA_FILES = 10L
    private const val INCREMENTAL = true
    private const val DEFAULT_BUILDUUID = "buildUuid"
    private const val DEFAULT_LANGUAGE_VERSION = "2.0.0"
  }
}
