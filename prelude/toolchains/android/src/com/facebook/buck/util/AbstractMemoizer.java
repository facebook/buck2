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

import com.facebook.buck.util.function.ThrowingSupplier;
import com.facebook.buck.util.types.Either;
import com.google.common.base.Preconditions;
import com.google.common.base.Throwables;
import javax.annotation.Nullable;

/** A Generic memoizer capable of memoizing Exceptions */
class AbstractMemoizer<T, E extends Exception> {

  @Nullable private volatile Either<T, E> value = null;

  /**
   * Get the value and memoize the result. Constructs the value if it hasn't been memoized before,
   * or if the previously memoized value has been collected.
   *
   * @param delegate delegate for constructing the value
   * @return the value
   */
  public T get(ThrowingSupplier<T, E> delegate, Class<E> exceptionClass) throws E {
    if (value == null) {
      synchronized (this) {
        if (value == null) {
          try {
            T t = Preconditions.checkNotNull(delegate.get());
            value = Either.ofLeft(t);
            return t;
          } catch (Exception e) {
            if (exceptionClass.isInstance(e)) {
              value = Either.ofRight(exceptionClass.cast(e));
              Throwables.throwIfInstanceOf(e, exceptionClass);
            }
            Throwables.throwIfUnchecked(e);
            throw new IllegalStateException(e);
          }
        }
      }
    }
    if (value.isLeft()) {
      return value.getLeft();
    }
    throw value.getRight();
  }
}
