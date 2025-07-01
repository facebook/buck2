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

import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.hamcrest.Matchers;
import org.junit.Test;

public class SimpleTestSelectorTest {

  @Test
  public void matchesSimpleDescription() {
    String suiteName = "Suite";
    String methodName = "method";
    TestSelector testSelector = new SimpleTestSelector(suiteName, methodName);
    TestDescription desc = new TestDescription(suiteName, methodName);
    assertThat(desc, new TestSelectorMatcher(testSelector));
  }

  @Test
  public void matchesDescriptionWithWeirdChars() {
    String suiteName = "Suite";
    String methodName = "method[\n\r]";
    TestSelector testSelector = new SimpleTestSelector(suiteName, methodName);
    TestDescription desc = new TestDescription(suiteName, methodName);
    assertThat(desc, new TestSelectorMatcher(testSelector));
  }

  @Test
  public void doesNotMatchWhenDifferentMethod() {
    String suiteName = "Suite";
    TestSelector testSelector = new SimpleTestSelector(suiteName, "method1");
    TestDescription desc = new TestDescription(suiteName, "method2");
    assertThat(desc, Matchers.not(new TestSelectorMatcher(testSelector)));
  }

  @Test
  public void doesNotMatchWhenDifferentSuite() {
    String methodName = "method";
    TestSelector testSelector = new SimpleTestSelector("Suite1", methodName);
    TestDescription desc = new TestDescription("Suite2", methodName);
    assertThat(desc, Matchers.not(new TestSelectorMatcher(testSelector)));
  }

  @Test
  public void shouldMatchClassPathWithOrWithoutInnerClass() {
    TestSelector selector = new SimpleTestSelector("com.example.Foo", "");
    assertTrue(selector.containsClassPath("com.example.Foo"));
    assertFalse(selector.containsClassPath("Foo"));

    selector = new SimpleTestSelector("com.example.Foo$Inner", "");
    assertTrue(selector.containsClassPath("com.example.Foo"));
    assertFalse(selector.containsClassPath("Foo"));
  }

  @Test
  public void shouldMatchOnlyNestedClass() {
    TestSelector selector = new SimpleTestSelector("com.example.Foo$InnerTest", "");
    assertTrue(selector.matchesClassName("com.example.Foo$InnerTest"));
    assertFalse(selector.matchesClassName("com.example.Foo"));
    assertFalse(selector.matchesClassName("com.example.Foo$OtherInner"));
    assertFalse(selector.matchesClassName("com.Foo$InnerTest"));
  }
}
