/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
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
