/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.core.exceptions;

import static com.facebook.buck.util.string.MoreStrings.linesToText;

import com.google.common.collect.ImmutableMap;
import java.util.regex.Pattern;
import org.junit.Assert;
import org.junit.Test;

public class HumanReadableExceptionAugmentorTest {

  @Test
  public void addsErrorMessageIfInvalidRegexProvided() {
    HumanReadableExceptionAugmentor augmentor =
        new HumanReadableExceptionAugmentor(
            ImmutableMap.of(
                Pattern.compile("Replace (.*) with something else"), "Should replace $1"));
    String error = augmentor.getAugmentedError("Replace foo bar baz with something else");
    Assert.assertEquals(
        linesToText("Replace foo bar baz with something else", "Should replace foo bar baz"),
        error);
  }

  @Test
  public void returnsOriginalMessageIfNoConfigGiven() {
    Assert.assertEquals(
        "foo bar!",
        new HumanReadableExceptionAugmentor(ImmutableMap.of()).getAugmentedError("foo bar!"));
  }

  @Test
  public void printsErrorIfInvalidRegexProvided() {
    HumanReadableExceptionAugmentor augmentor =
        new HumanReadableExceptionAugmentor(
            ImmutableMap.of(
                Pattern.compile("Replace (.*) with something else"), "Should replace $9"));
    String error = augmentor.getAugmentedError("Replace foo bar baz with something else");
    Assert.assertEquals(
        linesToText(
            "Replace foo bar baz with something else",
            "Could not replace text \"Replace foo bar baz with something else\" with regex \"Should"
                + " replace $9\": No group 9"),
        error);
  }

  @Test
  public void removesColorProperly() {
    // Sample output from clang that has some red, bold, resets, etc in it
    String coloredString =
        linesToText(
            "\u001B[1mmain.cpp:1:13: \u001B[0m\u001B[0;1;31merror: \u001B[0m\u001B[1mexpected"
                + " '}'\u001B[0m",
            "int main() {",
            "\u001B[0;1;32m            ^",
            "\u001B[0m\u001B[1mmain.cpp:1:12: \u001B[0m\u001B[0;1;30mnote: \u001B[0mto match this"
                + " '{'\u001B[0m",
            "int main() {",
            "\u001B[0;1;32m           ^",
            "\u001B[0m1 error generated.");
    String expected = coloredString + System.lineSeparator() + "Try adding '}'!";
    HumanReadableExceptionAugmentor augmentor =
        new HumanReadableExceptionAugmentor(
            ImmutableMap.of(
                Pattern.compile("main.cpp:1:13: error: expected ('}')"), "Try adding $1!"));
    Assert.assertEquals(expected, augmentor.getAugmentedError(coloredString));
  }
}
