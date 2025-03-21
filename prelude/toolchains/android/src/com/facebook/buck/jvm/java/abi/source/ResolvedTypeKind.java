/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java.abi.source;

enum ResolvedTypeKind {
  /** Type would resolve successfully. */
  RESOLVED_TYPE,
  /** Type would resolve as an {@link javax.lang.model.type.ErrorType}. */
  ERROR_TYPE,
  /** The compiler would crash trying to resolve the type. */
  CRASH;

  public ResolvedTypeKind merge(ResolvedTypeKind other) {
    return (this.compareTo(other) >= 0) ? this : other;
  }
}
