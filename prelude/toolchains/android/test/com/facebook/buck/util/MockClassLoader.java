/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.util;

import com.google.common.collect.ImmutableMap;

public class MockClassLoader extends ClassLoader {
  private final ImmutableMap<String, Class<?>> injectedClasses;

  public MockClassLoader(ClassLoader parent, ImmutableMap<String, Class<?>> injectedClasses) {
    super(parent);
    this.injectedClasses = injectedClasses;
  }

  @Override
  protected Class<?> findClass(String name) throws ClassNotFoundException {
    Class<?> found = injectedClasses.get(name);
    if (found != null) {
      return found;
    }

    return super.findClass(name);
  }
}
