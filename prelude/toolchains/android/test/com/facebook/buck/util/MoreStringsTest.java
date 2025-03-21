/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import com.facebook.buck.util.string.MoreStrings;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import java.util.regex.Pattern;
import org.junit.Test;

public class MoreStringsTest {

  @Test
  public void testIsEmpty() {
    assertTrue(MoreStrings.isEmpty(""));
    assertTrue(MoreStrings.isEmpty(new StringBuilder()));
    assertFalse(MoreStrings.isEmpty("text"));
    assertFalse(MoreStrings.isEmpty(new StringBuilder("text")));
  }

  @Test(expected = NullPointerException.class)
  public void testIsEmptyRejectsNull() {
    MoreStrings.isEmpty(null);
  }

  @Test
  public void testWithoutSuffix() {
    assertEquals("abc", MoreStrings.withoutSuffix("abcdef", "def"));
    assertEquals("", MoreStrings.withoutSuffix("abcdef", "abcdef"));
  }

  @Test
  public void testCapitalize() {
    assertEquals("Foo", MoreStrings.capitalize("foo"));
    assertEquals("F", MoreStrings.capitalize("f"));
    assertEquals("", MoreStrings.capitalize(""));
  }

  @Test(expected = NullPointerException.class)
  public void testCapitalizeRejectsNull() {
    MoreStrings.capitalize(null);
  }

  @Test
  public void testGetLevenshteinDistance() {
    assertEquals(
        "The distance between '' and 'BUILD' should be 5 (e.g., 5 insertions).",
        5,
        MoreStrings.getLevenshteinDistance("", "BUILD"));
    assertEquals(
        "'BUILD' and 'BUILD' should be identical.",
        0,
        MoreStrings.getLevenshteinDistance("BUILD", "BUILD"));
    assertEquals(
        "The distance between 'BIULD' and 'BUILD' should be 2 (e.g., 1 deletion + 1 insertion).",
        2,
        MoreStrings.getLevenshteinDistance("BIULD", "BUILD"));
    assertEquals(
        "The distance between 'INSTALL' and 'AUDIT' should be 7 "
            + "(e.g., 5 substitutions + 2 deletions).",
        7,
        MoreStrings.getLevenshteinDistance("INSTALL", "AUDIT"));
    assertEquals(
        "The distance between 'aaa' and 'bbbbbb' should be 6 "
            + "(e.g., 3 substitutions + 3 insertions).",
        6,
        MoreStrings.getLevenshteinDistance("aaa", "bbbbbb"));
    assertEquals(
        "The distance between 'build' and 'biuldd' should be 3 (e.g., 1 deletion + 2 insertions).",
        3,
        MoreStrings.getLevenshteinDistance("build", "biuldd"));
    assertEquals(
        "The distance between 'test' and 'tset' should be 2 (e.g., 1 deletion + 1 insertion).",
        2,
        MoreStrings.getLevenshteinDistance("test", "tset"));
    assertEquals(
        "The distance between '' and 'BUILD' should be 5 (e.g., 5 insertions).",
        5,
        MoreStrings.getLevenshteinDistance("BUILD", ""));
  }

  @Test
  public void testRegexPatternForAny() {
    Pattern varArgTestPattern = Pattern.compile(MoreStrings.regexPatternForAny("foo", "bar"));
    assertTrue(varArgTestPattern.matcher("bar").matches());
    assertTrue(varArgTestPattern.matcher("foo").matches());
    assertFalse(varArgTestPattern.matcher("mehfoo").matches());

    Pattern iterabeArgTestPattern =
        Pattern.compile(".*" + MoreStrings.regexPatternForAny(ImmutableSet.of("raz", "meh")) + "$");
    assertTrue(iterabeArgTestPattern.matcher("hello raz").matches());
    assertTrue(iterabeArgTestPattern.matcher("hello meh").matches());
    assertFalse(iterabeArgTestPattern.matcher("hello meh hi").matches());
  }

  @Test
  public void testEndsWithIgnoreCase() {
    assertTrue(MoreStrings.endsWithIgnoreCase("string.suffix", ".suffix"));
    assertTrue(MoreStrings.endsWithIgnoreCase("string.suffix", ".SUFFIX"));
    assertTrue(MoreStrings.endsWithIgnoreCase("string.SUFFIX", ".suffix"));
    assertTrue(MoreStrings.endsWithIgnoreCase("string.SUFFIX", ".SUFFIX"));
    assertTrue(MoreStrings.endsWithIgnoreCase("StrINg.SufFIx", ".SUffIX"));

    assertTrue(MoreStrings.endsWithIgnoreCase("string", "string"));
    assertTrue(MoreStrings.endsWithIgnoreCase("StRiNg", "STriNG"));
    assertTrue(MoreStrings.endsWithIgnoreCase("", ""));
    assertTrue(MoreStrings.endsWithIgnoreCase("string", ""));

    assertFalse(MoreStrings.endsWithIgnoreCase("string.something", ".suffix"));

    assertFalse(MoreStrings.endsWithIgnoreCase("string", "strin"));
    assertFalse(MoreStrings.endsWithIgnoreCase("strin", "string"));
  }

  @Test
  public void testSpellingSuggestionsWithinDistance() {
    assertEquals(
        MoreStrings.getSpellingSuggestions("appl:", ImmutableList.of("apple", "foo"), 2),
        ImmutableList.of("apple"));
  }

  @Test
  public void testSpellingSuggestionsOutsideOfDistance() {
    assertEquals(
        MoreStrings.getSpellingSuggestions("ppl:", ImmutableList.of("apple", "foo"), 1),
        ImmutableList.of());
  }

  @Test
  public void abbreviate() {
    assertEquals("a", MoreStrings.abbreviate("a", 2));
    assertEquals("abc", MoreStrings.abbreviate("abc", 3));
    assertEquals("...", MoreStrings.abbreviate("abcde", 2));
    assertEquals("...", MoreStrings.abbreviate("abcde", 2));
    assertEquals("...", MoreStrings.abbreviate("abcde", 3));
    assertEquals("a...", MoreStrings.abbreviate("abcde", 4));
    assertEquals("abcde", MoreStrings.abbreviate("abcde", 5));
    assertEquals("abcde", MoreStrings.abbreviate("abcde", 6));

    // also test it doesn't crash
    for (int width : new int[] {-10, -1, 0, 1, 3, 5, 20, 999999}) {
      for (String s : new String[] {"", "a", "ab", "abc", "abcd", "abcde", "abcdefghijklmn"}) {
        MoreStrings.abbreviate(s, width);
      }
    }
  }

  @Test
  public void linesToTextWithTrailingNewline() {
    assertEquals("", MoreStrings.linesToTextWithTrailingNewline(ImmutableList.of()));
    assertEquals(
        System.lineSeparator(), MoreStrings.linesToTextWithTrailingNewline(ImmutableList.of("")));
    assertEquals(
        "a" + System.lineSeparator() + System.lineSeparator(),
        MoreStrings.linesToTextWithTrailingNewline(ImmutableList.of("a", "")));
  }
}
