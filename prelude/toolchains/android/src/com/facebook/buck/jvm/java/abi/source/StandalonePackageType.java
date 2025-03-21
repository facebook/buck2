/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java.abi.source;

import java.util.List;
import javax.lang.model.element.AnnotationMirror;
import javax.lang.model.type.NoType;
import javax.lang.model.type.TypeKind;

public class StandalonePackageType extends StandaloneTypeMirror implements NoType {

  private final ArtificialPackageElement packageElement;

  public StandalonePackageType(
      ArtificialPackageElement packageElement, List<? extends AnnotationMirror> annotations) {
    super(TypeKind.PACKAGE, annotations);
    this.packageElement = packageElement;
  }

  public StandalonePackageType(ArtificialPackageElement packageElement) {
    super(TypeKind.PACKAGE);
    this.packageElement = packageElement;
  }

  public ArtificialPackageElement asElement() {
    return packageElement;
  }

  @Override
  public String toString() {
    return packageElement.getQualifiedName().toString();
  }

  @Override
  public StandalonePackageType cloneWithAnnotationMirrors(
      List<? extends AnnotationMirror> annotations) {
    return new StandalonePackageType(this.packageElement, annotations);
  }
}
