/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java.abi.source;

import com.facebook.buck.jvm.java.abi.source.api.CannotInferException;
import java.util.List;
import javax.lang.model.type.TypeMirror;

/** Returned as the type of an {@link InferredTypeElement}. */
class InferredDeclaredType extends StandaloneDeclaredType {
  public InferredDeclaredType(InferredTypeElement inferredTypeElement) {
    super(inferredTypeElement);
  }

  @Override
  public TypeMirror getEnclosingType() {
    throw new CannotInferException("enclosing type", this);
  }

  @Override
  public List<? extends TypeMirror> getTypeArguments() {
    throw new CannotInferException("type parameters", this);
  }
}
