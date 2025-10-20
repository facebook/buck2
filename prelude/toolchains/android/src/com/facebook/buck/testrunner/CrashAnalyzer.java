/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.testrunner;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Analyzes crash information from logcat output to detect specific crash types and provide enhanced
 * crash reporting.
 */
public class CrashAnalyzer {

  /** Defines a crash type with its detection pattern and message. */
  private static class CrashType {
    final Pattern pattern;
    final String detectionMessage;
    final boolean isSignal;

    CrashType(Pattern pattern, String detectionMessage, boolean isSignal) {
      this.pattern = pattern;
      this.detectionMessage = detectionMessage;
      this.isSignal = isSignal;
    }
  }

  private static final CrashType[] CRASH_TYPES = {
    new CrashType(
        Pattern.compile("java\\.lang\\.OutOfMemoryError:.*"),
        "Out of memory error detected",
        false),
  };

  /**
   * Analyzes logcat output for crash information and forwards it to stdout/stderr so the enhanced
   * crash detection system can detect specific crash types.
   *
   * @param logcatOutput The raw logcat output to analyze
   */
  public void analyzeCrashInformation(String logcatOutput) {
    if (logcatOutput == null || logcatOutput.trim().isEmpty()) {
      return;
    }

    System.err.println("=== AIT CRASH ANALYSIS ===");
    boolean crashDetected = false;

    for (CrashType crashType : CRASH_TYPES) {
      Matcher matcher = crashType.pattern.matcher(logcatOutput);
      if (matcher.find()) {
        crashDetected = true;
        System.err.println("AIT_CRASH_DETECTION: " + crashType.detectionMessage);

        if (crashType.isSignal) {
          extractSignalStackTrace(logcatOutput, matcher);
        } else {
          extractExceptionStackTrace(logcatOutput, matcher);
        }
      }
    }

    if (!crashDetected) {
      System.err.println("No crashes detected");
    }
    System.err.println("=== END AIT CRASH ANALYSIS ===");
  }

  /**
   * Extracts Java exception details and stack trace location.
   *
   * @param logcatOutput The raw logcat output
   * @param exceptionMatcher The matcher that found the Java exception
   */
  private void extractExceptionStackTrace(String logcatOutput, Matcher exceptionMatcher) {
    // Extract the Java exception line
    int matchStart = exceptionMatcher.start();
    int lineStart = logcatOutput.lastIndexOf('\n', matchStart) + 1;
    int lineEnd = logcatOutput.indexOf('\n', matchStart);
    if (lineEnd == -1) {
      lineEnd = logcatOutput.length();
    }

    String exceptionLine = logcatOutput.substring(lineStart, lineEnd).trim();
    if (!exceptionLine.isEmpty()) {
      System.err.println("Exception: " + exceptionLine);
    }

    // Look for the "at" stack trace line immediately following the exception
    int nextLineStart = lineEnd + 1;
    if (nextLineStart < logcatOutput.length()) {
      int nextLineEnd = logcatOutput.indexOf('\n', nextLineStart);
      if (nextLineEnd == -1) {
        nextLineEnd = logcatOutput.length();
      }

      String nextLine = logcatOutput.substring(nextLineStart, nextLineEnd).trim();
      // Check if the next line contains stack trace information starting with "at"
      if (nextLine.contains("\tat ") || nextLine.trim().startsWith("at ")) {
        System.err.println("Stack trace: " + nextLine.trim());
      }
    }
  }

  /**
   * Extracts signal crash details and backtrace location.
   *
   * @param logcatOutput The raw logcat output
   * @param signalMatcher The matcher that found the signal
   */
  private void extractSignalStackTrace(String logcatOutput, Matcher signalMatcher) {
    // Extract the signal line
    int matchStart = signalMatcher.start();
    int lineStart = logcatOutput.lastIndexOf('\n', matchStart) + 1;
    int lineEnd = logcatOutput.indexOf('\n', matchStart);
    if (lineEnd == -1) {
      lineEnd = logcatOutput.length();
    }

    String signalLine = logcatOutput.substring(lineStart, lineEnd).trim();
    if (!signalLine.isEmpty()) {
      System.err.println("Signal: " + signalLine);
    }

    // Look for additional signal information (like backtrace) in following lines
    int nextLineStart = lineEnd + 1;
    for (int i = 0; i < 5 && nextLineStart < logcatOutput.length(); i++) {
      int nextLineEnd = logcatOutput.indexOf('\n', nextLineStart);
      if (nextLineEnd == -1) {
        nextLineEnd = logcatOutput.length();
      }

      String nextLine = logcatOutput.substring(nextLineStart, nextLineEnd).trim();
      // Look for backtrace information or native crash details
      if (nextLine.contains("backtrace:") || nextLine.contains("pc ") || nextLine.contains("#")) {
        System.err.println("Backtrace: " + nextLine.trim());
      }
      nextLineStart = nextLineEnd + 1;
    }
  }
}
