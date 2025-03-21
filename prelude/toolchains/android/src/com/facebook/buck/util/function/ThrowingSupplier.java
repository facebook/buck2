/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.util.function;

import com.google.common.base.Throwables;
import java.util.function.Supplier;

/** This version of {@code Supplier<T>} can throw an exception. */
@FunctionalInterface
public interface ThrowingSupplier<T, E extends Exception> {
  T get() throws E;

  /** Returns a Supplier that will wrap any thrown exception in a RuntimeException. */
  default Supplier<T> asSupplier() {
    return () -> {
      try {
        return get();
      } catch (Exception e) {
        Throwables.throwIfUnchecked(e);
        throw new RuntimeException(e);
      }
    };
  }

  /** Returns a {@link ThrowingSupplier} from a {@link Supplier} */
  static <T, E extends Exception> ThrowingSupplier<T, E> fromSupplier(Supplier<T> supplier) {
    return () -> supplier.get();
  }
}
