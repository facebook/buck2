/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.testresultsoutput;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonGenerator;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Optional;

/**
 * TestResultsOutputEvent implements the following thrift schema.
 * https://www.internalfb.com/code/fbsource/[8bed6a02e7ed]/fbcode/testinfra/if/test_result_output_spec.thrift
 *
 * <p>It keeps and serializes events pertaining to the execution of test cases and replicates
 * instead of using the thrift structs directly to avoid the need for a dependency on fbcode.
 */
public class TestResultsOutputEvent {
  private static final JsonFactory jsonFactory = new JsonFactory();

  /** Represents a test event, which can be either a start or finish event. */
  public static class TestEvent {
    public StartEvent start;
    public FinishEvent finish;
  }

  /** Represents a start event for a test. */
  public static class StartEvent {
    public String name;

    /**
     * Creates a new start event with the given name.
     *
     * @param name the name of the test
     */
    public StartEvent(String name) {
      this.name = name;
    }

    /**
     * Serializes this start event to a JSON byte array.
     *
     * @return the JSON byte array
     * @throws IOException if there is an error serializing the event
     */
    public byte[] toJsonBytes() throws IOException {
      ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
      try (JsonGenerator generator = jsonFactory.createGenerator(outputStream)) {
        generator.writeStartObject();
        generator.writeObjectFieldStart("start");
        generator.writeStringField("name", name);
        generator.writeEndObject();
        generator.writeEndObject();
      }
      return outputStream.toByteArray();
    }
  }

  /** Represents the status of a test. */
  public enum TestStatus {
    PASS(0),
    FAIL(1),
    SKIP(2);

    private final int value;

    /**
     * Creates a new test status with the given value.
     *
     * @param value the integer value of the status
     */
    TestStatus(int value) {
      this.value = value;
    }

    /**
     * Returns the integer value of the status.
     *
     * @return the integer value of the status
     */
    public int getValue() {
      return value;
    }
  };

  /** Represents a finish event for a test. */
  public static class FinishEvent {
    public String name;
    public TestStatus status;

    /** The time at which the test ended, in milliseconds since Unix epoch. */
    public long endedTime;

    /** The duration of the test, in milliseconds since the start of the test. */
    public long duration;

    /** A message associated with the test result, or null if there is no message. */
    public String message;

    /**
     * Creates a new finish event with the given parameters.
     *
     * @param name the name of the test
     * @param status the status of the test
     * @param endedTime the time at which the test ended, in milliseconds since Unix epoch
     * @param duration the duration of the test, in milliseconds since the start of the test
     * @param message a message associated with the test result, or null if there is no message
     */
    public FinishEvent(
        String name, TestStatus status, long endedTime, long duration, Optional<String> message) {
      this.name = name;
      this.status = status;
      this.endedTime = endedTime;
      this.duration = duration;
      this.message = message.orElse(null);
    }

    /**
     * Serializes this finish event to a JSON byte array.
     *
     * @return the JSON byte array
     * @throws IOException if there is an error serializing the event
     */
    public byte[] toJsonBytes() throws IOException {
      ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
      try (JsonGenerator generator = jsonFactory.createGenerator(outputStream)) {
        generator.writeStartObject();
        generator.writeObjectFieldStart("finish");
        generator.writeStringField("name", name);
        generator.writeNumberField("status", status.getValue());
        generator.writeNumberField("duration", duration);
        if (message != null) {
          generator.writeStringField("message", message);
        }
        generator.writeNumberField("ended_time", endedTime);
        generator.writeEndObject();
        generator.writeEndObject();
      }
      return outputStream.toByteArray();
    }
  }
}
