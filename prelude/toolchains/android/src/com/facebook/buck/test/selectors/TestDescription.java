/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.test.selectors;

import java.util.Objects;

/**
 * A non-JUnit specific way of describing a test-method inside a test-class. This meants that the
 * test-selectors code does not need a dependency on JUnit.
 */
public class TestDescription {

  private final String className;
  private final String methodName;

  public TestDescription(String className, String methodName) {
    this.className = className;
    this.methodName = methodName;
  }

  public String getClassName() {
    return className;
  }

  public String getMethodName() {
    return methodName;
  }

  @Override
  public boolean equals(Object obj) {
    if (!(obj instanceof TestDescription)) {
      return false;
    }
    TestDescription other = (TestDescription) obj;
    return Objects.equals(this.className, other.className)
        && Objects.equals(this.methodName, other.methodName);
  }

  @Override
  public int hashCode() {
    return Objects.hash(this.className, this.methodName);
  }

  @Override
  public String toString() {
    return String.format("%s.%s", this.className, this.methodName);
  }
}
