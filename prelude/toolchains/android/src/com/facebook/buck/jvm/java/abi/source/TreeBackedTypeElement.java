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
import com.sun.source.tree.ClassTree;
import com.sun.source.util.TreePath;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementVisitor;
import javax.lang.model.element.Name;
import javax.lang.model.element.NestingKind;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.TypeParameterElement;
import javax.lang.model.type.TypeMirror;

/**
 * An implementation of {@link TypeElement} that uses only the information available from a {@link
 * ClassTree}. This results in an incomplete implementation; see documentation for individual
 * methods and {@link com.facebook.buck.jvm.java.abi.source} for more information.
 */
class TreeBackedTypeElement extends TreeBackedParameterizable implements ArtificialTypeElement {
  private final TreeBackedTypes types;
  private final TypeElement underlyingElement;
  private final ClassTree tree;
  @Nullable private StandaloneDeclaredType typeMirror;
  @Nullable private TypeMirror superclass;
  @Nullable private List<? extends TypeMirror> interfaces;

  TreeBackedTypeElement(
      TreeBackedTypes types,
      TypeElement underlyingElement,
      TreeBackedElement enclosingElement,
      TreePath treePath,
      PostEnterCanonicalizer canonicalizer) {
    super(underlyingElement, enclosingElement, treePath, canonicalizer);
    this.types = types;
    this.underlyingElement = underlyingElement;
    this.tree = (ClassTree) treePath.getLeaf();
    enclosingElement.addEnclosedElement(this);
  }

  @Override
  public void complete() {
    asType();
    getSuperclass();
    getInterfaces();
  }

  @Override
  TreePath getTreePath() {
    return Objects.requireNonNull(super.getTreePath());
  }

  @Override
  ClassTree getTree() {
    return tree;
  }

  @Override
  public TreeBackedElement getEnclosingElement() {
    return Objects.requireNonNull(super.getEnclosingElement());
  }

  @Override
  public void addEnclosedElement(Element element) {
    if (!(element instanceof TreeBackedElement)) {
      throw new IllegalArgumentException(
          String.format(
              "A type named %s does not exist. Make sure it is a canonical reference.", element));
    }
    super.addEnclosedElement(element);
  }

  @Override
  public List<TreeBackedElement> getEnclosedElements() {
    @SuppressWarnings("unchecked")
    List<TreeBackedElement> enclosedElements =
        (List<TreeBackedElement>) super.getEnclosedElements();
    return enclosedElements;
  }

  @Override
  public NestingKind getNestingKind() {
    return underlyingElement.getNestingKind();
  }

  @Override
  public Name getQualifiedName() {
    return underlyingElement.getQualifiedName();
  }

  @Override
  public StandaloneDeclaredType asType() {
    if (typeMirror == null) {
      typeMirror =
          (StandaloneDeclaredType)
              types.getDeclaredType(
                  this,
                  getTypeParameters().stream()
                      .map(TypeParameterElement::asType)
                      .toArray(TypeMirror[]::new));
    }
    return typeMirror;
  }

  @Override
  public TypeMirror getSuperclass() {
    if (superclass == null) {
      superclass =
          getCanonicalizer()
              .getCanonicalType(
                  underlyingElement.getSuperclass(), getTreePath(), tree.getExtendsClause());
    }

    return superclass;
  }

  @Override
  public List<? extends TypeMirror> getInterfaces() {
    if (interfaces == null) {
      interfaces =
          Collections.unmodifiableList(
              getCanonicalizer()
                  .getCanonicalTypes(
                      underlyingElement.getInterfaces(),
                      getTreePath(),
                      tree.getImplementsClause()));
    }
    return interfaces;
  }

  @Override
  public <R, P> R accept(ElementVisitor<R, P> v, P p) {
    return v.visitType(this, p);
  }
}
