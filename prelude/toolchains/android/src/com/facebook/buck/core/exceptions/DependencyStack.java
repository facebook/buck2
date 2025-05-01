/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.core.exceptions;

import com.google.common.base.Joiner;
import com.google.common.collect.ImmutableList;
import javax.annotation.Nullable;

/**
 * Stack-trace for providing better diagnostics when working with dependency graph.
 *
 * <p>This type must not be used for anything except for providing error diagnostics.
 */
public final class DependencyStack {
  /**
   * Marker interface for stack trace elements. It is a marker to avoid accidental storing of
   * objects of inappropriate types in the trace.
   */
  public interface Element extends ProvidesElement {
    @Override
    default Element getElement() {
      return this;
    }

    default String elementToString() {
      return toString();
    }
  }

  /** Types which have something to be stored in stack trace */
  public interface ProvidesElement {
    Element getElement();
  }

  /** Tail stack or {@code null} for an empty stack */
  @Nullable private final DependencyStack parent;

  /** Last element. Undefined for empty stack */
  private final Object where;

  private DependencyStack(@Nullable DependencyStack parent, Object where) {
    this.parent = parent;
    this.where = where;
  }

  private static final DependencyStack ROOT = new DependencyStack(null, new Element() {});

  /** Create an empty stack */
  public static DependencyStack root() {
    return ROOT;
  }

  /** Create a single element stack */
  public static DependencyStack top(Element top) {
    return root().child(top);
  }

  /** Create a single element stack */
  public static DependencyStack top(String top) {
    return root().child(top);
  }

  /** Cons */
  public DependencyStack child(Element where) {
    return new DependencyStack(this, where);
  }

  /** Cons */
  public DependencyStack child(String where) {
    return new DependencyStack(this, where);
  }

  /** Check if stack is empty */
  public boolean isEmpty() {
    return this == ROOT;
  }

  /** Collect the stack elements, top first */
  public ImmutableList<Object> collect() {
    ImmutableList.Builder<Object> builder = ImmutableList.builder();

    DependencyStack next = this;
    while (next.parent != null) {
      builder.add(next.where);
      next = next.parent;
    }

    return builder.build();
  }

  private static String elementToString(Object o) {
    if (o == null) {
      return "null";
    } else if (o instanceof Element) {
      return ((Element) o).elementToString();
    } else {
      return o.toString();
    }
  }

  /**
   * Collect the stack elements, top first, converting all elements to strings, and filtering out
   * adjacent duplicates.
   */
  public ImmutableList<String> collectStringsFilterAdjacentDupes() {
    ImmutableList.Builder<String> builder = ImmutableList.builder();

    DependencyStack next = this;
    String prevString = null;
    while (next.parent != null) {
      String string = elementToString(next.where);
      if (prevString == null || !prevString.equals(string)) {
        builder.add(string);
      }
      prevString = string;
      next = next.parent;
    }

    return builder.build();
  }

  // All DependencyStack objects are equal to each other to avoid accidental
  // errors when DependencyStack objects are accidentally a field of data class.

  @Override
  public boolean equals(Object o) {
    return o instanceof DependencyStack;
  }

  @Override
  public int hashCode() {
    return 0;
  }

  @Override
  public String toString() {
    return "[" + Joiner.on(", ").join(collect()) + "]";
  }
}
