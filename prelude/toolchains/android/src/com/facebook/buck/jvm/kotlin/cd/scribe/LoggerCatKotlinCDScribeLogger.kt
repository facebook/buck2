/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.kotlin.cd.scribe

import com.facebook.buck.core.util.log.Logger
import java.io.ByteArrayOutputStream
import java.io.IOException
import java.util.Base64
import org.apache.thrift.protocol.TCompactProtocol
import org.apache.thrift.transport.TIOStreamTransport

object LoggerCatKotlinCDScribeLogger : KotlinCDScribeLogger {

  private val logger: Logger = Logger.get(LoggerCatKotlinCDScribeLogger::class.java.name)

  override fun log(logEntry: KotlinCDLogEntry): Boolean {
    return try {
      val logEntryThrift = getThriftLogEntry(logEntry)
      val outputStream = ByteArrayOutputStream()
      val protocol = TCompactProtocol(TIOStreamTransport(outputStream))
      logEntryThrift.write(protocol)
      val bytesToSend = outputStream.toByteArray()
      val logEntryB64Encoded = Base64.getEncoder().encodeToString(bytesToSend)
      val processBuilder =
          ProcessBuilder(
              "logger_cat", "KotlinCDLoggerConfig", logEntryB64Encoded, "--base64", "-v1")
      val process = processBuilder.start()
      val exitCode = process.waitFor()
      if (exitCode != 0) {
        logger.warn("logger_cat exited with non-zero status: $exitCode")
        false
      } else {
        true
      }
    } catch (e: IOException) {
      logger.warn("Exception while running logger_cat: ${e.message}")
      false
    } catch (e: InterruptedException) {
      logger.warn("Exception while running logger_cat: ${e.message}")
      false
    }
  }

  private fun getThriftLogEntry(
      logEntry: KotlinCDLogEntry
  ): com.facebook.buck.jvm.kotlin.cd.scribe.model.KotlinCDLogEntry {
    return com.facebook.buck.jvm.kotlin.cd.scribe.model.KotlinCDLogEntry().apply {
      setTime(logEntry.time)
      setEvent_time(logEntry.eventTime)
      setTarget(logEntry.target)
      setSubtarget(logEntry.subtarget)
      setBuild_uuid(logEntry.buildUuid)
      setExecution_platform(logEntry.executionPlatform)
      setNum_kotlin_files(logEntry.numKotlinFiles)
      setNum_java_files(logEntry.numJavaFiles)
      setIncremental(logEntry.incremental)
      setKotlinc_mode(logEntry.kotlincMode)
      setClasspath_changes(logEntry.classpathChanges)
      setStep(logEntry.step)
      setLanguage_version(logEntry.languageVersion)
      setExtras(logEntry.extras)
    }
  }
}
