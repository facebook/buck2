/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java.lang.model;

import javax.lang.model.element.ExecutableElement;

/**
 * A bridge method is used in Java to translate certain language constructs (like overriding methods
 * with covariant return types) into a form that can be understood by the VM.
 */
public class BridgeMethod {

  /**
   * For a normal override bridge, this is the overriding method. For an accessibility bridge, this
   * is the same as {@link #to}.
   */
  public final ExecutableElement from;

  /**
   * For a normal override bridge, this is the method being overridden. For an accessibility bridge,
   * this is a public method in a non-public superclass of the current class.
   */
  public final ExecutableElement to;

  public BridgeMethod(ExecutableElement from, ExecutableElement to) {
    this.from = from;
    this.to = to;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }

    BridgeMethod that = (BridgeMethod) o;

    if (!from.equals(that.from)) {
      return false;
    }
    return to.equals(that.to);
  }

  @Override
  public int hashCode() {
    int result = from.hashCode();
    result = 31 * result + to.hashCode();
    return result;
  }

  @Override
  public String toString() {
    return "BridgeMethod{"
        + "from="
        + from.getEnclosingElement()
        + "."
        + from
        + ", to="
        + to.getEnclosingElement()
        + "."
        + to
        + '}';
  }

  /**
   * Bridge methods may be generated for public methods of non-public superclasses to (as the
   * compiler source puts it) "work around a horrible but permanent reflection design error."
   *
   * @return true if this is such a bridge method
   */
  public boolean isAccessibilityBridge() {
    return from == to;
  }

  /**
   * Bridge methods may be generated when an overriding method has a different erasure than the
   * overridden method, or a different return type. (The VM only sees erased types, and at the VM
   * level overloading on return type is legal, so these bridge methods are necessary to implement
   * overriding as the Java language specifies it.)
   *
   * @return true if this is such a bridge method
   */
  public boolean isOverrideBridge() {
    return from != to;
  }
}
