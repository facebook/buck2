/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.util.types;

/**
 * Unit type.
 *
 * <p>It is similar to {@link Void} types, except that {@link Void} types cannot be instantiated.
 * This type can be used in generic code, for example, when generic code is not meant to return
 * anything, but {@code null} is not desirable or not allowed.
 */
public enum Unit {
  UNIT
}
