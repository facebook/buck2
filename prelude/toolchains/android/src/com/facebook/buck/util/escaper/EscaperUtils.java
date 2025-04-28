/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.util.escaper;

import com.google.common.base.CharMatcher;
import java.util.function.Function;

/** Utility functions used for escaping */
public class EscaperUtils {
  /**
   * Escapes the special characters identified the {@link CharMatcher}, using single quotes.
   *
   * @param matcher identifies characters to be escaped
   * @param str string to quote
   * @return possibly quoted string
   */
  public static String escape(Quoter quoter, CharMatcher matcher, String str) {
    if (matcher.matchesAnyOf(str) || str.isEmpty()) {
      return quoter.quote(str);
    } else {
      return str;
    }
  }

  /**
   * @return a escaper function using the given quote style and escaping characters determined by
   *     the given matcher.
   */
  public static Function<String, String> escaper(Quoter quoter, CharMatcher matcher) {
    return input -> escape(quoter, matcher, input);
  }
}
