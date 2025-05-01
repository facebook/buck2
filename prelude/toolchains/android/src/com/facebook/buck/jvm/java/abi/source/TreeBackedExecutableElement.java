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
import com.sun.source.tree.MethodTree;
import com.sun.source.tree.Tree;
import com.sun.source.tree.VariableTree;
import com.sun.source.util.TreePath;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import javax.lang.model.element.AnnotationValue;
import javax.lang.model.element.ElementVisitor;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.TypeParameterElement;
import javax.lang.model.element.VariableElement;
import javax.lang.model.type.TypeMirror;
import javax.lang.model.type.TypeVariable;

/**
 * An implementation of {@link ExecutableElement} that uses only the information available from a
 * {@link MethodTree}. This results in an incomplete implementation; see documentation for
 * individual methods and {@link com.facebook.buck.jvm.java.abi.source} for more information.
 */
class TreeBackedExecutableElement extends TreeBackedParameterizable
    implements ArtificialExecutableElement {
  private final ExecutableElement underlyingElement;
  private final List<TreeBackedVariableElement> parameters = new ArrayList<>();
  @Nullable private final MethodTree tree;

  @Nullable private TypeMirror returnType;
  @Nullable private TypeMirror receiverType;
  @Nullable private List<TypeMirror> thrownTypes;
  @Nullable private TreeBackedAnnotationValue defaultValue;
  @Nullable private StandaloneExecutableType typeMirror;

  TreeBackedExecutableElement(
      ExecutableElement underlyingElement,
      TreeBackedElement enclosingElement,
      @Nullable TreePath treePath,
      PostEnterCanonicalizer canonicalizer) {
    super(underlyingElement, enclosingElement, treePath, canonicalizer);
    this.underlyingElement = underlyingElement;
    this.tree = treePath == null ? null : (MethodTree) treePath.getLeaf();
    enclosingElement.addEnclosedElement(this);
  }

  @Override
  public List<? extends ArtificialElement> getEnclosedElements() {
    return Collections.emptyList();
  }

  @Override
  @Nullable
  MethodTree getTree() {
    return tree;
  }

  @Override
  public void complete() {
    asType();
    getReturnType();
    getReceiverType();
    getThrownTypes();
    getDefaultValue();
  }

  @Override
  public StandaloneTypeMirror asType() {
    if (typeMirror == null) {
      typeMirror =
          new StandaloneExecutableType(
              getReturnType(),
              getTypeParameters().stream()
                  .map(TypeParameterElement::asType)
                  .map(type -> (TypeVariable) type)
                  .collect(Collectors.toList()),
              getParameters().stream().map(VariableElement::asType).collect(Collectors.toList()),
              getThrownTypes(),
              getAnnotationMirrors());
    }
    return typeMirror;
  }

  @Override
  public TypeMirror getReturnType() {
    if (returnType == null) {
      returnType =
          getCanonicalizer()
              .getCanonicalType(
                  underlyingElement.getReturnType(),
                  getTreePath(),
                  tree == null ? null : tree.getReturnType());
    }
    return returnType;
  }

  @Override
  public List<TreeBackedVariableElement> getParameters() {
    return Collections.unmodifiableList(parameters);
  }

  /* package */ void addParameter(TreeBackedVariableElement parameter) {
    parameters.add(parameter);
  }

  @Override
  @Nullable
  public TypeMirror getReceiverType() {
    if (receiverType == null) {
      TreePath methodTreePath = getTreePath();
      TreePath receiverParameterPath = null;
      Tree receiverTypeTree = null;
      if (methodTreePath != null && tree != null) {
        VariableTree receiverParameterTree = tree.getReceiverParameter();
        if (receiverParameterTree != null) {
          receiverParameterPath = new TreePath(methodTreePath, receiverParameterTree);
          receiverTypeTree = receiverParameterTree.getType();
        }
      }
      receiverType =
          getCanonicalizer()
              .getCanonicalType(
                  underlyingElement.getReceiverType(), receiverParameterPath, receiverTypeTree);
    }
    return receiverType;
  }

  @Override
  public boolean isVarArgs() {
    return underlyingElement.isVarArgs();
  }

  @Override
  public boolean isDefault() {
    return underlyingElement.isDefault();
  }

  @Override
  public List<? extends TypeMirror> getThrownTypes() {
    if (thrownTypes == null) {
      thrownTypes =
          Collections.unmodifiableList(
              getCanonicalizer()
                  .getCanonicalTypes(
                      underlyingElement.getThrownTypes(),
                      getTreePath(),
                      tree == null ? null : tree.getThrows()));
    }

    return thrownTypes;
  }

  @Override
  @Nullable
  public TreeBackedAnnotationValue getDefaultValue() {
    if (defaultValue == null) {
      AnnotationValue underlyingValue = underlyingElement.getDefaultValue();
      if (underlyingValue != null) {
        defaultValue =
            new TreeBackedAnnotationValue(
                underlyingValue,
                new TreePath(
                    Objects.requireNonNull(getTreePath()),
                    Objects.requireNonNull(tree).getDefaultValue()),
                getCanonicalizer());
      }
    }
    return defaultValue;
  }

  @Override
  public <R, P> R accept(ElementVisitor<R, P> v, P p) {
    return v.visitExecutable(this, p);
  }
}
