/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.test.selectors;

import org.hamcrest.Description;
import org.hamcrest.TypeSafeMatcher;

/** A Hamcrest matcher that matches when the {@link TestSelector} matches. */
public class TestSelectorMatcher extends TypeSafeMatcher<TestDescription> {
  private final TestSelector testSelector;

  public TestSelectorMatcher(TestSelector testSelector) {
    super(TestDescription.class);
    this.testSelector = testSelector;
  }

  @Override
  protected boolean matchesSafely(TestDescription testDescription) {
    return testSelector.matches(testDescription);
  }

  @Override
  public void describeTo(Description description) {
    description.appendText(String.format("Matches testSelector %s", testSelector.getRawSelector()));
  }
}
