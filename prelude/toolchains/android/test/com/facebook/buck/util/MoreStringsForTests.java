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

import org.hamcrest.Matcher;
import org.hamcrest.core.IsEqual;
import org.hamcrest.core.StringContains;

public class MoreStringsForTests {
  /** Utility class: do not instantiate. */
  private MoreStringsForTests() {}

  private static class IsEqualIgnoringPlatformNewlines extends IsEqual<String> {
    public IsEqualIgnoringPlatformNewlines(String expectedText) {
      super(normalizeNewlines(expectedText));
    }

    @Override
    public boolean matches(Object o) {
      if (o instanceof String) {
        String candidateText = normalizeNewlines((String) o);
        return super.matches(candidateText);
      }
      return false;
    }
  }

  /** A cross-platform matcher for a string with newlines. */
  public static Matcher<String> equalToIgnoringPlatformNewlines(String expectedText) {
    return new IsEqualIgnoringPlatformNewlines(expectedText);
  }

  private static class ContainsIgnoringPlatformNewlines extends StringContains {
    public ContainsIgnoringPlatformNewlines(String expectedText) {
      super(false, normalizeNewlines(expectedText));
    }

    @Override
    protected boolean evalSubstringOf(String s) {
      return this.converted(normalizeNewlines(s)).contains(this.converted(this.substring));
    }
  }

  /** A cross-platform matcher for a string with newlines. */
  public static Matcher<String> containsIgnoringPlatformNewlines(String expectedText) {
    return new ContainsIgnoringPlatformNewlines(expectedText);
  }

  public static String normalizeNewlines(String s) {
    // Windows uses "\r\n", some mac apps use "\r", so standardize on "\n".
    return s.replace("\r\n", "\n").replace('\r', '\n');
  }
}
