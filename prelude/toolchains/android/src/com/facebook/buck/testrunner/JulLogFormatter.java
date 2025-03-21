/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.testrunner;

import com.facebook.buck.core.util.log.appendablelogrecord.AppendableLogRecord;
import java.io.PrintWriter; // NOPMD can't depend on Guava
import java.io.StringWriter;
import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Locale;
import java.util.logging.Formatter;
import java.util.logging.Level;
import java.util.logging.LogRecord;

public class JulLogFormatter extends Formatter {
  private static final int ERROR_LEVEL = Level.SEVERE.intValue();
  private static final int WARN_LEVEL = Level.WARNING.intValue();
  private static final int INFO_LEVEL = Level.INFO.intValue();
  private static final int DEBUG_LEVEL = Level.FINE.intValue();
  private static final int VERBOSE_LEVEL = Level.FINER.intValue();
  private static final DateTimeFormatter DATE_FORMAT =
      DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS", Locale.US)
          .withZone(ZoneId.systemDefault());

  @Override
  public String format(LogRecord record) {
    long timeMillis = record.getMillis();
    Instant instant = Instant.ofEpochMilli(timeMillis);
    String timestamp = DATE_FORMAT.format(instant);

    // We explicitly don't use String.format here because this code is very
    // performance-critical: http://stackoverflow.com/a/1281651
    long tid = record.getThreadID();
    StringBuilder sb =
        new StringBuilder("[")
            .append(timestamp)
            .append("]")
            .append(formatRecordLevel(record.getLevel()))
            .append("[tid:");
    // Zero-pad on the left. We're currently assuming we have less than 100 threads.
    if (tid < 10) {
      sb.append("0").append(tid);
    } else {
      sb.append(tid);
    }
    sb.append("][").append(record.getLoggerName()).append("] ");
    if (record instanceof AppendableLogRecord) {
      ((AppendableLogRecord) record).appendFormattedMessage(sb);
    } else {
      sb.append(formatMessage(record));
    }
    sb.append("\n");
    Throwable t = record.getThrown();
    if (t != null) {
      // Closing a StringWriter has no effect, so we don't need to do it in a
      // try-with-resources (but oddly, it throws IOException, so we couldn't
      // use it in a try-with-resources if we wanted to).
      StringWriter sw = new StringWriter();
      try (PrintWriter pw = new PrintWriter(sw)) { // NOPMD can't depend on Guava
        t.printStackTrace(pw);
        sb.append(sw).append("\n");
      }
    }
    return sb.toString();
  }

  private static String formatRecordLevel(Level level) {
    int l = level.intValue();
    if (l == ERROR_LEVEL) {
      return "[error]";
    } else if (l == WARN_LEVEL) {
      return "[warn ]";
    } else if (l == INFO_LEVEL) {
      return "[info ]";
    } else if (l == DEBUG_LEVEL) {
      return "[debug]";
    } else if (l == VERBOSE_LEVEL) {
      return "[vrbos]";
    } else {
      // We don't expect this to happen, so meh, let's use String.format for simplicity.
      return String.format("[%-5d]", l);
    }
  }
}
