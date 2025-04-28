/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java.abi.source;

import java.util.Collections;
import java.util.Set;
import javax.lang.model.element.TypeElement;

/**
 * Represents a resolved type as simulated by {@link
 * com.facebook.buck.jvm.java.abi.source.CompilerTypeResolutionSimulator}.
 */
class ResolvedType {
  public final ResolvedTypeKind kind;
  public final TypeElement type;
  public final Set<String> missingDependencies;

  public ResolvedType(ResolvedTypeKind kind, TypeElement type) {
    this(kind, type, Collections.emptySet());
  }

  public ResolvedType(ResolvedTypeKind kind, TypeElement type, Set<String> missingDependencies) {
    this.kind = kind;
    this.type = type;
    this.missingDependencies = missingDependencies;
  }
}
