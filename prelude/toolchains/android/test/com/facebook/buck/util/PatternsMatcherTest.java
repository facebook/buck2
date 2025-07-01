/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import com.google.common.collect.ImmutableList;
import java.util.Map;
import java.util.TreeMap;
import org.junit.Test;

public class PatternsMatcherTest {

  @Test
  public void testMatchesPattern() {
    PatternsMatcher patternsMatcher =
        new PatternsMatcher(ImmutableList.of("pattern.*", "test_pattern"));

    assertTrue(patternsMatcher.matches("pattern"));
    assertTrue(patternsMatcher.matches("test_pattern"));
    assertTrue(patternsMatcher.substringMatches("pattern"));
    assertTrue(patternsMatcher.substringMatches("test_pattern"));
  }

  @Test
  public void testMatchesAnyWithExactMatch() {
    PatternsMatcher patternsMatcher =
        new PatternsMatcher(ImmutableList.of("pattern.*", "test_pattern"));

    assertTrue(patternsMatcher.matches("test_pattern"));
  }

  @Test
  public void testMatchesAnyWithWildcard() {
    PatternsMatcher patternsMatcher =
        new PatternsMatcher(ImmutableList.of("pattern.*", "test_pattern"));

    assertTrue(patternsMatcher.matches("pattern"));
  }

  @Test
  public void testDoesNotMatchPrefix() {
    PatternsMatcher patternsMatcher = new PatternsMatcher(ImmutableList.of("test"));

    assertFalse(patternsMatcher.matches("test_pattern"));
  }

  @Test
  public void testMatchAnyWithNonMatchingPrefixReturnsFalse() {
    PatternsMatcher patternsMatcher = new PatternsMatcher(ImmutableList.of("test"));

    assertFalse(patternsMatcher.matches("test_pattern"));
  }

  @Test
  public void testSubstringMatchesPrefix() {
    PatternsMatcher patternsMatcher = new PatternsMatcher(ImmutableList.of("test"));

    assertTrue(patternsMatcher.substringMatches("test_pattern"));
  }

  @Test
  public void testDoesNotMatchSuffix() {
    PatternsMatcher patternsMatcher = new PatternsMatcher(ImmutableList.of("pattern"));

    assertFalse(patternsMatcher.matches("test_pattern"));
  }

  @Test
  public void testSubstringMatchesSuffix() {
    PatternsMatcher patternsMatcher = new PatternsMatcher(ImmutableList.of("pattern"));

    assertTrue(patternsMatcher.substringMatches("test_pattern"));
  }

  @Test
  public void testDoesNotMatchInfix() {
    PatternsMatcher patternsMatcher = new PatternsMatcher(ImmutableList.of("_"));

    assertFalse(patternsMatcher.matches("test_pattern"));
  }

  @Test
  public void testSubstringMatchesInfix() {
    PatternsMatcher patternsMatcher = new PatternsMatcher(ImmutableList.of("_"));

    assertTrue(patternsMatcher.substringMatches("test_pattern"));
  }

  @Test
  public void testDoesNotMatchPattern() {
    PatternsMatcher patternsMatcher =
        new PatternsMatcher(ImmutableList.of("pattern.*", "test_pattern"));

    assertFalse(patternsMatcher.matches("wrong_pattern"));
    assertFalse(patternsMatcher.substringMatches("wrong_pat"));
  }

  @Test
  public void testMatchesAnyDoesNotMatchPattern() {
    PatternsMatcher patternsMatcher =
        new PatternsMatcher(ImmutableList.of("pattern.*", "test_pattern"));

    assertFalse(patternsMatcher.matches("wrong_pattern"));
  }

  @Test
  public void testMatchesAnyDoesNotMatchEmptyPatterns() {
    assertFalse(PatternsMatcher.NONE.matches("wrong_pattern"));
  }

  @Test
  public void testMatchesMatchesEmptyPatterns() {
    assertTrue(PatternsMatcher.ANY.matches("wrong_pattern"));
  }

  @Test
  public void testHasPatterns() {
    PatternsMatcher patternsMatcher =
        new PatternsMatcher(ImmutableList.of("pattern.*", "test_pattern"));

    assertFalse(patternsMatcher.isMatchesAny());
    assertFalse(patternsMatcher.isMatchesNone());
  }

  @Test
  public void testHasNoPatterns() {
    PatternsMatcher patternsMatcher = new PatternsMatcher(ImmutableList.of());

    assertTrue(patternsMatcher.isMatchesNone());
    assertFalse(patternsMatcher.isMatchesAny());
  }

  @Test
  public void testFilterMatchingMapEntriesWithEmptyPatterns() {
    PatternsMatcher patternsMatcher = PatternsMatcher.ANY;

    Map<String, String> entries =
        new TreeMap<String, String>() {
          {
            put("e1", "v1");
            put("e2", "v2");
            put("e3", "v3");
          }
        };

    assertEquals(entries, patternsMatcher.filterMatchingMapKeys(entries));
  }

  @Test
  public void testFilterMatchingMapEntries() {
    PatternsMatcher patternsMatcher = new PatternsMatcher(ImmutableList.of("e1", "e2"));

    Map<String, String> entries =
        new TreeMap<String, String>() {
          {
            put("e1", "v1");
            put("e2", "v2");
            put("e3", "v3");
          }
        };

    Map<String, String> expectedEntries =
        new TreeMap<String, String>() {
          {
            put("e1", "v1");
            put("e2", "v2");
          }
        };

    assertEquals(expectedEntries, patternsMatcher.filterMatchingMapKeys(entries));
  }
}
