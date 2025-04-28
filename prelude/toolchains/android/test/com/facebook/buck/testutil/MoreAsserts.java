/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.testutil;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.not;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Joiner;
import com.google.common.base.Preconditions;
import com.google.common.collect.Iterables;
import com.google.common.collect.Sets;
import com.google.common.io.ByteStreams;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.annotation.Nullable;
import org.hamcrest.Matcher;
import org.hamcrest.Matchers;
import org.junit.Assert;

/** Additional assertions that delegate to JUnit assertions, but with better error messages. */
public final class MoreAsserts {

  private static final int BUFFER_SIZE = 8 * 1024;

  private MoreAsserts() {}

  /**
   * Asserts that two sets have the same contents. On failure, prints a readable diff of the two
   * sets for easy debugging.
   */
  public static <E> void assertSetEquals(Set<E> expected, Set<E> actual) {
    Set<E> missing = Sets.difference(expected, actual);
    Set<E> extra = Sets.difference(actual, expected);
    boolean setsEqual = missing.isEmpty() && extra.isEmpty();
    Assert.assertTrue(
        String.format("%nMissing elements:%n%s%nExtraneous elements:%n%s", missing, extra),
        setsEqual);
  }

  /**
   * @see #assertIterablesEquals(Iterable, Iterable)
   */
  public static <T extends List<?>> void assertListEquals(List<?> expected, List<?> observed) {
    assertIterablesEquals(expected, observed);
  }

  /**
   * @see #assertIterablesEquals(String, Iterable, Iterable)
   */
  public static <T extends List<?>> void assertListEquals(
      String userMessage, List<?> expected, List<?> observed) {
    assertIterablesEquals(userMessage, expected, observed);
  }

  /**
   * Equivalent to {@link org.junit.Assert#assertEquals(Object, Object)} except if the assertion
   * fails, the message includes information about where the iterables differ.
   */
  public static <T extends Iterable<?>> void assertIterablesEquals(
      Iterable<?> expected, Iterable<?> observed) {
    assertIterablesEquals("" /* userMessage */, expected, observed);
  }

  /**
   * Equivalent to {@link org.junit.Assert#assertEquals(String, Object, Object)} except if the
   * assertion fails, the message includes information about where the iterables differ.
   */
  public static <T extends Iterable<?>> void assertIterablesEquals(
      String userMessage, Iterable<?> expected, Iterable<?> observed) {
    // The traditional assertEquals() method should be fine if either List is null.
    if (expected == null || observed == null) {
      assertEquals(userMessage, expected, observed);
      return;
    }

    String errmsgPart =
        String.format(
            "expected:[%s] observed:[%s]",
            Joiner.on(", ").join(expected), Joiner.on(", ").join(observed));

    // Compare each item in the list, one at a time.
    Iterator<?> expectedIter = expected.iterator();
    Iterator<?> observedIter = observed.iterator();
    int index = 0;
    while (expectedIter.hasNext()) {
      if (!observedIter.hasNext()) {
        fail(
            prefixWithUserMessage(
                userMessage,
                "Item %d does not exist in the observed list (%s): %s",
                index,
                errmsgPart,
                expectedIter.next()));
      }
      Object expectedItem = expectedIter.next();
      Object observedItem = observedIter.next();
      assertEquals(
          prefixWithUserMessage(
              userMessage, "Item %d in the lists should match (%s).", index, errmsgPart),
          expectedItem,
          observedItem);
      ++index;
    }
    if (observedIter.hasNext()) {
      fail(
          prefixWithUserMessage(
              userMessage,
              "Extraneous item %d in the observed list (%s): %s.",
              index,
              errmsgPart,
              observedIter.next()));
    }
  }

  public static <Item, Container extends Iterable<Item>> void assertContainsOne(
      Container container, Item expectedItem) {
    assertContainsOne(/* userMessage */ Iterables.toString(container), container, expectedItem);
  }

  public static <Item, Container extends Iterable<Item>> void assertContainsOne(
      String userMessage, Container container, Item expectedItem) {
    int seen = 0;
    for (Item item : container) {
      if (expectedItem.equals(item)) {
        seen++;
      }
    }
    if (seen < 1) {
      failWith(
          userMessage,
          "Item '" + expectedItem + "' not found in container, " + "expected to find one.");
    }
    if (seen > 1) {
      failWith(
          userMessage,
          "Found "
              + seen
              + " occurrences of '"
              + expectedItem
              + "' in container, expected to find only one.");
    }
  }

  public static <T> void assertOptionalValueEquals(
      String userMessage, T expectedValue, Optional<T> optionalValue) {
    if (!optionalValue.isPresent()) {
      failWith(userMessage, "Optional value is not present.");
    }

    assertEquals(userMessage, expectedValue, optionalValue.get());
  }

  public static void assertContentsEqual(Path one, Path two) throws IOException {
    Preconditions.checkNotNull(one);
    Preconditions.checkNotNull(two);

    if (one.equals(two)) {
      return;
    }

    if (Files.size(one) != Files.size(two)) {
      fail(
          String.format(
              "File sizes differ: %s (%d bytes), %s (%d bytes)",
              one, Files.size(one), two, Files.size(two)));
    }

    try (InputStream ois = Files.newInputStream(one);
        InputStream tis = Files.newInputStream(two)) {
      byte[] bo = new byte[BUFFER_SIZE];
      byte[] bt = new byte[BUFFER_SIZE];

      while (true) {
        int read1 = ByteStreams.read(ois, bo, 0, BUFFER_SIZE);
        int read2 = ByteStreams.read(tis, bt, 0, BUFFER_SIZE);
        if (read1 != read2 || !Arrays.equals(bo, bt)) {
          fail(String.format("Contents of files differ: %s, %s", one, two));
        } else if (read1 != BUFFER_SIZE) {
          return;
        }
      }
    }
  }

  /**
   * Asserts that two strings are equal, but compares them in chunks so that Intellij will show the
   * diffs when the assertion fails.
   */
  public static void assertLargeStringsEqual(String expected, String content) {
    List<String> expectedChunks = chunkify(expected);
    List<String> contentChunks = chunkify(content);

    for (int i = 0; i < Math.min(expectedChunks.size(), contentChunks.size()); i++) {
      assertEquals("Failed at index: " + i, expectedChunks.get(i), contentChunks.get(i));
    }
    // We could check this first, but it's usually more useful to see the first difference than to
    // just see that the two strings are different length.
    assertEquals(expectedChunks.size(), contentChunks.size());
  }

  private static List<String> chunkify(String data) {
    return Stream.of(Iterables.partition(Arrays.asList(data.split("\\n")), 1000))
        .map((l) -> Joiner.on("\n").join(l))
        .collect(Collectors.toList());
  }

  private static String prefixWithUserMessage(
      @Nullable String userMessage, String message, Object... formatArgs) {
    return (userMessage == null ? "" : userMessage + " ") + String.format(message, formatArgs);
  }

  private static void failWith(@Nullable String userMessage, String message) {
    fail(prefixWithUserMessage(userMessage, message));
  }

  public static void assertJsonMatches(String expectedJson, String actualJson) throws IOException {
    assertJsonMatches(expectedJson, actualJson, Matchers::equalTo, "JSON outputs are not equal");
  }

  public static void assertJsonNotMatches(String expectedJson, String actualJson)
      throws IOException {
    assertJsonMatches(expectedJson, actualJson, s -> not(equalTo(s)), "JSON outputs are equal");
  }

  public static void assertJsonMatches(
      String expectedJson,
      String actualJson,
      Function<String, Matcher<String>> matcherGenerator,
      String message)
      throws IOException {
    ObjectMapper mapper = new ObjectMapper();
    Object observedValue = mapper.readValue(actualJson, Object.class);
    String actual = mapper.writerWithDefaultPrettyPrinter().writeValueAsString(observedValue);

    Object expectedValue = mapper.readValue(expectedJson, Object.class);
    String expected = mapper.writerWithDefaultPrettyPrinter().writeValueAsString(expectedValue);

    assertThat(message, actual, matcherGenerator.apply(expected));
  }
}
