/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.testutil;

import com.google.common.collect.ImmutableList;
import java.util.logging.Handler;
import java.util.logging.LogRecord;
import org.hamcrest.FeatureMatcher;
import org.hamcrest.Matcher;
import org.junit.rules.TestRule;
import org.junit.runner.Description;
import org.junit.runners.model.Statement;

/**
 * A LogSink that buffers log messages written to a particular logger.
 *
 * <p>Uses JUnit's rule mechanism to take care of installing and removing itself as a Logger
 * handler.
 *
 * <p>Usage:
 *
 * <pre>
 *   {@code @Rule}
 *   public final logSink = new TestLogSink(ClassUnderTest.class);
 * </pre>
 */
public final class TestLogSink extends Handler implements TestRule {

  private final ImmutableList.Builder<LogRecord> records = ImmutableList.builder();
  private final String loggerNameUnderTest;

  /**
   * Construct a log sink that listens to log message from a particular class's logger.
   *
   * @param classUnderTest Class whose logger should be listened to
   */
  public TestLogSink(Class<?> classUnderTest) {
    this.loggerNameUnderTest = classUnderTest.getName();
  }

  /**
   * Construct a log sink that listens to log message from a particular logger.
   *
   * @param loggerNameUnderTest Logger that should be listened to
   */
  public TestLogSink(String loggerNameUnderTest) {
    this.loggerNameUnderTest = loggerNameUnderTest;
  }

  /** Retrieve the log records that were published. */
  public synchronized ImmutableList<LogRecord> getRecords() {
    return records.build();
  }

  /**
   * Construct a hamcrest matcher that matches on the {@code LogRecord}'s message field.
   *
   * @param messageMatcher Matcher for the message.
   */
  public static Matcher<LogRecord> logRecordWithMessage(Matcher<String> messageMatcher) {
    return new FeatureMatcher<>(messageMatcher, "a LogRecord with message", "message") {
      @Override
      protected String featureValueOf(LogRecord logRecord) {
        return logRecord.getMessage();
      }
    };
  }

  @Override
  public Statement apply(Statement base, Description description) {
    return new Statement() {
      @Override
      public void evaluate() throws Throwable {
        java.util.logging.Logger logger = java.util.logging.Logger.getLogger(loggerNameUnderTest);
        logger.addHandler(TestLogSink.this);
        try {
          base.evaluate();
        } finally {
          logger.removeHandler(TestLogSink.this);
        }
      }
    };
  }

  @Override
  public synchronized void publish(LogRecord record) {
    if (record != null) {
      records.add(record);
    }
  }

  @Override
  public void flush() {}

  @Override
  public void close() throws SecurityException {}
}
