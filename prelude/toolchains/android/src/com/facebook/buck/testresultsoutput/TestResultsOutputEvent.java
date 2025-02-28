/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.testresultsoutput;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.Optional;

/**
 * TestResultsOutputEvent implements the following thrift schema.
 * https://www.internalfb.com/code/fbsource/[8bed6a02e7ed]/fbcode/testinfra/if/test_result_output_spec.thrift
 *
 * <p>It keeps and serializes events pertaining to the execution of test cases and replicates
 * instead of using the thrift structs directly to avoid the need for a dependency on fbcode.
 */
public class TestResultsOutputEvent {
  private static ObjectMapper mapper = new ObjectMapper();

  /** Represents a test event, which can be either a start or finish event. */
  @JsonInclude(Include.NON_NULL)
  public static class TestEvent {
    public StartEvent start;
    public FinishEvent finish;

    /**
     * Creates a new test event from a start event.
     *
     * @param start the start event
     * @return the new test event
     */
    public static TestEvent fromStartEvent(StartEvent start) {
      TestEvent event = new TestEvent();
      event.start = start;
      return event;
    }

    /**
     * Creates a new test event from a finish event.
     *
     * @param finish the finish event
     * @return the new test event
     */
    public static TestEvent fromFinishEvent(FinishEvent finish) {
      TestEvent event = new TestEvent();
      event.finish = finish;
      return event;
    }
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
     * @throws JsonProcessingException if there is an error serializing the event
     */
    public byte[] toJsonBytes() throws JsonProcessingException {
      return mapper.writeValueAsBytes(TestEvent.fromStartEvent(this));
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
    @JsonValue
    public int value() {
      return value;
    }
  };

  /** Represents a finish event for a test. */
  @JsonInclude(Include.NON_NULL)
  public static class FinishEvent {
    public String name;
    public TestStatus status;

    /** The time at which the test ended, in milliseconds since Unix epoch. */
    @JsonProperty("ended_time")
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
     * @throws JsonProcessingException if there is an error serializing the event
     */
    public byte[] toJsonBytes() throws JsonProcessingException {
      return mapper.writeValueAsBytes(TestEvent.fromFinishEvent(this));
    }
  }
}
