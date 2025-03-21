/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java.abi.source;

import com.facebook.buck.util.liteinfersupport.Nullable;
import java.util.List;
import javax.lang.model.element.AnnotationMirror;
import javax.lang.model.type.TypeKind;
import javax.lang.model.type.TypeMirror;
import javax.lang.model.type.WildcardType;

/**
 * An implementation of {@link WildcardType} that is not dependent on any particular compiler
 * implementation. It requires a {@link TypeMirror}, but does not depend on any particular
 * implementation of it (beyond the spec).
 */
class StandaloneWildcardType extends StandaloneTypeMirror implements WildcardType {

  @Nullable private final TypeMirror extendsBound;
  @Nullable private final TypeMirror superBound;

  public StandaloneWildcardType(
      @Nullable TypeMirror extendsBound, @Nullable TypeMirror superBound) {
    super(TypeKind.WILDCARD);
    this.extendsBound = extendsBound;
    this.superBound = superBound;
  }

  public StandaloneWildcardType(
      @Nullable TypeMirror extendsBound,
      @Nullable TypeMirror superBound,
      List<? extends AnnotationMirror> annotations) {
    super(TypeKind.WILDCARD, annotations);
    this.extendsBound = extendsBound;
    this.superBound = superBound;
  }

  @Override
  @Nullable
  public TypeMirror getExtendsBound() {
    return extendsBound;
  }

  @Override
  @Nullable
  public TypeMirror getSuperBound() {
    return superBound;
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();

    builder.append('?');
    if (extendsBound != null) {
      builder.append(" extends ");
      builder.append(extendsBound);
    } else if (superBound != null) {
      builder.append(" super ");
      builder.append(superBound);
    }

    return builder.toString();
  }

  @Override
  public StandaloneTypeMirror cloneWithAnnotationMirrors(
      List<? extends AnnotationMirror> newAnnotations) {
    return new StandaloneWildcardType(this.extendsBound, this.superBound, newAnnotations);
  }
}
