/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import com.google.common.collect.ImmutableSet;
import java.util.regex.Pattern;
import java.util.zip.ZipEntry;
import org.junit.Test;

public class RemoveClassesPatternsMatcherTest {
  @Test
  public void testEmptyMatcherRemovesNothing() {
    RemoveClassesPatternsMatcher matcher = RemoveClassesPatternsMatcher.EMPTY;

    assertFalse(matcher.test("com.example.Foo"));
    assertFalse(matcher.test(new ZipEntry("com/example/Foo.class")));
  }

  @Test
  public void testStringMatcher() {
    RemoveClassesPatternsMatcher matcher =
        new RemoveClassesPatternsMatcher(ImmutableSet.of(Pattern.compile("com[.]example[.]Foo")));

    assertTrue(matcher.test("com.example.Foo"));
  }

  @Test
  public void testZipEntryMatcher() {
    RemoveClassesPatternsMatcher matcher =
        new RemoveClassesPatternsMatcher(ImmutableSet.of(Pattern.compile("com[.]example[.]Foo")));

    assertTrue(matcher.test(new ZipEntry("com/example/Foo.class")));
  }

  @Test
  public void testNeverMatchesNonClasses() {
    RemoveClassesPatternsMatcher patternsMatcher =
        new RemoveClassesPatternsMatcher(ImmutableSet.of(Pattern.compile("test")));

    assertFalse(patternsMatcher.test(new ZipEntry("com/example/Foo/Foo.txt")));
  }

  @Test
  public void testMatchesPrefix() {
    RemoveClassesPatternsMatcher patternsMatcher =
        new RemoveClassesPatternsMatcher(ImmutableSet.of(Pattern.compile("test")));

    assertTrue(patternsMatcher.test("test_pattern"));
  }

  @Test
  public void testMatchesSuffix() {
    RemoveClassesPatternsMatcher patternsMatcher =
        new RemoveClassesPatternsMatcher(ImmutableSet.of(Pattern.compile("pattern")));

    assertTrue(patternsMatcher.test("test_pattern"));
  }

  @Test
  public void testMatchesInfix() {
    RemoveClassesPatternsMatcher patternsMatcher =
        new RemoveClassesPatternsMatcher(ImmutableSet.of(Pattern.compile("_")));

    assertTrue(patternsMatcher.test("test_pattern"));
  }

  @Test
  public void testExplicitMatchFullPatternSuccess() {
    RemoveClassesPatternsMatcher patternsMatcher =
        new RemoveClassesPatternsMatcher(ImmutableSet.of(Pattern.compile("^test_pattern$")));

    assertTrue(patternsMatcher.test("test_pattern"));
  }

  @Test
  public void testExplicitMatchFullPatternFailure() {
    RemoveClassesPatternsMatcher patternsMatcher =
        new RemoveClassesPatternsMatcher(ImmutableSet.of(Pattern.compile("^test_pattern$")));

    assertFalse(patternsMatcher.test("test_patterns"));
  }
}
