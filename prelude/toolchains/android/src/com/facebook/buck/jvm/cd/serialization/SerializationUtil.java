/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.cd.serialization;

/** CD serialization utilities. */
public class SerializationUtil {

  private SerializationUtil() {}

  public static RuntimeException createNotSupportedException(Enum<?> value) {
    return new IllegalStateException(value.name() + " enum value is not supported!");
  }
}
