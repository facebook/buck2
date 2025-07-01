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

import static org.junit.Assert.fail;

import java.util.function.Supplier;
import org.junit.Assert;
import org.junit.Test;

public class MoreSuppliersTest {
  @Test
  public void memoizingSupplierShouldMemoizeResult() {
    Supplier<Object> supplier = MoreSuppliers.memoize(Object::new);
    Object a = supplier.get();
    Object b = supplier.get();
    Assert.assertSame("Supplier should have cached the instance", a, b);
  }

  @Test
  public void memoizingSupplierShouldMemoizeRuntimeException() {
    Supplier<Object> supplier =
        MoreSuppliers.memoize(
            () -> {
              throw new RuntimeException();
            });
    try {
      supplier.get();
      fail("Expected runtime exception");
    } catch (RuntimeException e1) {
      try {
        supplier.get();
        fail("Expected runtime exception");
      } catch (RuntimeException e2) {
        Assert.assertSame("Supplier should have cached the instance", e1, e2);
      }
    }
  }
}
