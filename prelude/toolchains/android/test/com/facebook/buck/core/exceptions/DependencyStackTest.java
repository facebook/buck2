/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.core.exceptions;

import com.google.common.collect.ImmutableList;
import org.junit.Assert;
import org.junit.Test;

public class DependencyStackTest {
  private enum ElementImpl implements DependencyStack.Element {
    A,
    B,
    C,
    D,
  }

  @Test
  public void collect() {
    Assert.assertEquals(ImmutableList.of(), DependencyStack.root().collect());
    Assert.assertEquals(
        ImmutableList.of(ElementImpl.A), DependencyStack.top(ElementImpl.A).collect());
    Assert.assertEquals(
        ImmutableList.of(ElementImpl.A), DependencyStack.root().child(ElementImpl.A).collect());
    Assert.assertEquals(
        ImmutableList.of(ElementImpl.A), DependencyStack.root().child(ElementImpl.A).collect());
    Assert.assertEquals(
        ImmutableList.of(ElementImpl.B, ElementImpl.A),
        DependencyStack.root().child(ElementImpl.A).child(ElementImpl.B).collect());
  }

  @Test
  public void collectStringsFilterAdjacentDupes() {
    Assert.assertEquals(
        ImmutableList.of(), DependencyStack.root().collectStringsFilterAdjacentDupes());
    Assert.assertEquals(
        ImmutableList.of("A"),
        DependencyStack.top(ElementImpl.A).collectStringsFilterAdjacentDupes());
    Assert.assertEquals(
        ImmutableList.of("B", "A"),
        DependencyStack.top(ElementImpl.A)
            .child(ElementImpl.B)
            .collectStringsFilterAdjacentDupes());
    Assert.assertEquals(
        ImmutableList.of("A"),
        DependencyStack.top(ElementImpl.A)
            .child(ElementImpl.A)
            .collectStringsFilterAdjacentDupes());
    Assert.assertEquals(
        ImmutableList.of("B", "A"),
        DependencyStack.top(ElementImpl.A)
            .child(ElementImpl.A)
            .child(ElementImpl.B)
            .collectStringsFilterAdjacentDupes());
    Assert.assertEquals(
        ImmutableList.of("B", "A"),
        DependencyStack.top(ElementImpl.A)
            .child(ElementImpl.B)
            .child(ElementImpl.B)
            .collectStringsFilterAdjacentDupes());
  }

  private enum ElementImplWithCustomToString implements DependencyStack.Element {
    A,
    B,
    ;

    @Override
    public String elementToString() {
      return name() + "!";
    }
  }

  @Test
  public void withCustomToString() {
    ImmutableList<String> strings =
        DependencyStack.root()
            .child(ElementImplWithCustomToString.A)
            .child(ElementImplWithCustomToString.B)
            .collectStringsFilterAdjacentDupes();
    Assert.assertEquals(ImmutableList.of("B!", "A!"), strings);
  }
}
