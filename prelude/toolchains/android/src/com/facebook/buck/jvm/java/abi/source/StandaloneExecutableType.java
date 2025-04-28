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
import javax.lang.model.type.ExecutableType;
import javax.lang.model.type.TypeKind;
import javax.lang.model.type.TypeMirror;
import javax.lang.model.type.TypeVariable;

/**
 * An implementation of {@link ExecutableType} that is not dependent on any particular compiler
 * implementation. It requires {@link javax.lang.model.element.Element} and {@link TypeMirror}
 * objects, but does not depend on any particular implementation of them (beyond the spec).
 */
class StandaloneExecutableType extends StandaloneTypeMirror implements ExecutableType {

  private final TypeMirror returnType;
  private final List<? extends TypeVariable> typeVariables;
  private final List<? extends TypeMirror> parameterTypes;
  private final List<? extends TypeMirror> thrownTypes;

  public StandaloneExecutableType(
      TypeMirror returnType,
      List<? extends TypeVariable> typeVariables,
      List<? extends TypeMirror> parameterTypes,
      List<? extends TypeMirror> thrownTypes,
      List<? extends AnnotationMirror> annotations) {
    super(TypeKind.EXECUTABLE, annotations);
    this.returnType = returnType;
    this.typeVariables = List.copyOf(typeVariables);
    this.parameterTypes = List.copyOf(parameterTypes);
    this.thrownTypes = List.copyOf(thrownTypes);
  }

  @Override
  public List<? extends TypeVariable> getTypeVariables() {
    return typeVariables;
  }

  @Override
  public TypeMirror getReturnType() {
    return returnType;
  }

  @Override
  public List<? extends TypeMirror> getParameterTypes() {
    return parameterTypes;
  }

  @Override
  @Nullable
  public TypeMirror getReceiverType() {
    // TODO(jkeljo): Either implement this adaptively or wait until we eliminate javac 7 support
    return null;
  }

  @Override
  public List<? extends TypeMirror> getThrownTypes() {
    return thrownTypes;
  }

  @Override
  public StandaloneExecutableType cloneWithAnnotationMirrors(
      List<? extends AnnotationMirror> annotations) {
    throw new UnsupportedOperationException("Cannot add annotations to a method type");
  }
}
