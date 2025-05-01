/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.util;

import com.facebook.buck.util.function.ThrowingSupplier;
import java.util.function.Supplier;

/**
 * Memoizes a value, supporting passing in a supplier when getting the value, unlike {@link
 * MoreSuppliers.MemoizingSupplier}.
 */
public class Memoizer<T> extends AbstractMemoizer<T, RuntimeException> {

  public T get(Supplier<T> delegate) {
    return get(ThrowingSupplier.fromSupplier(delegate), RuntimeException.class);
  }
}
