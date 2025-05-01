/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.util.string;

import com.facebook.buck.core.util.log.Logger;
import com.facebook.buck.util.types.Pair;
import com.google.common.base.Joiner;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableList;
import com.google.common.io.CharSource;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;

public final class MoreStrings {

  private static final Logger LOG = Logger.get(MoreStrings.class);

  private static final int KEEP_CHARS_DEFAULT = 10_000;
  private static final String TRUNCATE_MESSAGE =
      "...\n<truncated>\n...".replace("\n", System.lineSeparator());

  /** Utility class: do not instantiate. */
  private MoreStrings() {}

  /**
   * Joins the lines passed in with the platform line separator.
   *
   * @param lines the lines that need to be joined.
   * @return String, containing the joined lines using the platform line separator as delimiter.
   */
  public static String linesToText(String... lines) {
    return String.join(System.lineSeparator(), lines);
  }

  /**
   * Joins the lines passed in with the platform line separator.
   *
   * @param lines the lines that need to be joined.
   * @return String, containing the joined lines using the platform line separator as delimiter.
   */
  public static String linesToTextWithTrailingNewline(List<String> lines) {
    if (lines.isEmpty()) {
      return "";
    }
    if (lines.size() == 1) {
      return lines.get(0) + System.lineSeparator();
    }
    StringBuilder sb = new StringBuilder();
    for (String line : lines) {
      sb.append(line).append(System.lineSeparator());
    }
    return sb.toString();
  }

  public static boolean isEmpty(CharSequence sequence) {
    return sequence.length() == 0;
  }

  public static String withoutSuffix(String str, String suffix) {
    Preconditions.checkArgument(str.endsWith(suffix), "%s must end with %s", str, suffix);
    return str.substring(0, str.length() - suffix.length());
  }

  public static String capitalize(String str) {
    if (!str.isEmpty()) {
      return str.substring(0, 1).toUpperCase() + str.substring(1);
    } else {
      return "";
    }
  }

  public static int getLevenshteinDistance(String str1, String str2) {
    if (str1.length() < str2.length()) {
      // ensure that str2 is always the smallest one to make space complexity O(min(|str1|, |str2|)
      return getLevenshteinDistance(str2, str1);
    }

    // reduce memory usage by storing only the last 2 rows of the table
    int previous = 0;
    int[][] levenshteinDist = new int[2][str2.length() + 1];

    for (int j = 0; j <= str2.length(); j++) {
      levenshteinDist[previous][j] = j;
    }

    for (int i = 1; i <= str1.length(); i++, previous = 1 - previous) {
      int current = 1 - previous;
      levenshteinDist[current][0] = i;
      for (int j = 1; j <= str2.length(); j++) {
        if (str1.charAt(i - 1) == str2.charAt(j - 1)) {
          levenshteinDist[current][j] = levenshteinDist[previous][j - 1];
        } else {
          levenshteinDist[current][j] =
              Math.min(
                      levenshteinDist[previous][j],
                      Math.min(levenshteinDist[current][j - 1], levenshteinDist[previous][j - 1]))
                  + 1;
        }
      }
    }

    return levenshteinDist[previous][str2.length()];
  }

  public static String regexPatternForAny(String... values) {
    return regexPatternForAny(Arrays.asList(values));
  }

  public static String regexPatternForAny(Iterable<String> values) {
    return "((?:" + Joiner.on(")|(?:").join(values) + "))";
  }

  public static boolean endsWithIgnoreCase(String str, String suffix) {
    if (str.length() < suffix.length()) {
      return false;
    }

    return str.substring(str.length() - suffix.length()).equalsIgnoreCase(suffix);
  }

  public static Optional<String> stripPrefix(String s, String prefix) {
    return s.startsWith(prefix) ? Optional.of(s.substring(prefix.length())) : Optional.empty();
  }

  public static Optional<String> stripSuffix(String s, String suffix) {
    return s.endsWith(suffix)
        ? Optional.of(s.substring(0, s.length() - suffix.length()))
        : Optional.empty();
  }

  public static String truncatePretty(String data) {
    return truncateMiddle(data, KEEP_CHARS_DEFAULT, KEEP_CHARS_DEFAULT);
  }

  public static String truncateTail(String data, int keepFirstChars) {
    return truncateMiddle(data, keepFirstChars, 0, TRUNCATE_MESSAGE);
  }

  public static String truncateMiddle(String data, int keepFirstChars, int keepLastChars) {
    return truncateMiddle(data, keepFirstChars, keepLastChars, TRUNCATE_MESSAGE);
  }

  public static String truncateMiddle(
      String data, int keepFirstChars, int keepLastChars, String truncateMessage) {
    if (data.length() <= keepFirstChars + keepLastChars + truncateMessage.length()) {
      return data;
    }
    LOG.verbose("Before truncate: " + data);

    StringBuilder resultBuilder = new StringBuilder(data.substring(0, keepFirstChars));
    resultBuilder.append(truncateMessage);
    if (keepLastChars > 0) {
      resultBuilder.append(data.substring(data.length() - keepLastChars));
    }
    return resultBuilder.toString();
  }

  /** Split string into lines. */
  public static ImmutableList<String> lines(String data) {
    if (data.isEmpty()) {
      return ImmutableList.of();
    }
    try {
      return CharSource.wrap(data).readLines();
    } catch (IOException e) {
      // unreachable
      throw new RuntimeException(e);
    }
  }

  /** Compare two strings lexicographically. */
  public static int compareStrings(String a, String b) {
    if (a == b) {
      return 0;
    }
    return a.compareTo(b);
  }

  /**
   * @return The spelling suggestion for the {@code input} based on its Levenstein distance from a
   *     list of available {@code options}.
   */
  public static List<String> getSpellingSuggestions(
      String input, Collection<String> options, int maxDistance) {
    return options.stream()
        .map(option -> new Pair<>(option, MoreStrings.getLevenshteinDistance(input, option)))
        .filter(pair -> pair.getSecond() <= maxDistance)
        .sorted(Comparator.comparing(Pair::getSecond))
        .map(Pair::getFirst)
        .collect(ImmutableList.toImmutableList());
  }

  /** If string width exceeds passed parameter, replace string tail with dot-dot-dot. */
  public static String abbreviate(String s, int width) {
    String dotDotDot = "...";
    if (s.length() <= width) {
      return s;
    } else if (width <= dotDotDot.length()) {
      return dotDotDot;
    } else {
      return s.substring(0, width - dotDotDot.length()) + dotDotDot;
    }
  }
}
