/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.foo;

public class Dependency<T> {
  public class Inner {
    public class Innerer {}
  }

  public class NonGenericInner {
    public class GenericInnerer<U> {}
  }

  public class GenericInner<U> {}
}
