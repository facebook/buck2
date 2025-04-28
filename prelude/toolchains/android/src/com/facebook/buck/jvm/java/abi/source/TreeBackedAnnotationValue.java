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
import com.sun.source.tree.AssignmentTree;
import com.sun.source.tree.Tree;
import com.sun.source.util.TreePath;
import java.util.List;
import java.util.stream.Collectors;
import javax.lang.model.element.AnnotationValue;
import javax.lang.model.element.AnnotationValueVisitor;
import javax.lang.model.element.VariableElement;
import javax.lang.model.type.TypeMirror;
import javax.lang.model.util.SimpleAnnotationValueVisitor8;

class TreeBackedAnnotationValue implements ArtificialAnnotationValue {
  private final AnnotationValue underlyingAnnotationValue;
  private final TreePath treePath;
  private final Tree valueTree;
  private final PostEnterCanonicalizer canonicalizer;

  @Nullable private Object value;

  TreeBackedAnnotationValue(
      AnnotationValue underlyingAnnotationValue,
      TreePath treePath,
      PostEnterCanonicalizer canonicalizer) {
    this.underlyingAnnotationValue = underlyingAnnotationValue;
    Tree tree = treePath.getLeaf();
    if (tree instanceof AssignmentTree) {
      AssignmentTree assignmentTree = (AssignmentTree) tree;
      valueTree = assignmentTree.getExpression();
      this.treePath = new TreePath(treePath, valueTree);
    } else {
      valueTree = tree;
      this.treePath = treePath;
    }
    this.canonicalizer = canonicalizer;
  }

  /* package */ AnnotationValue getUnderlyingAnnotationValue() {
    return underlyingAnnotationValue;
  }

  @Override
  public Object getValue() {
    if (value == null) {
      value = canonicalizer.getCanonicalValue(underlyingAnnotationValue, treePath);
    }
    return value;
  }

  @Override
  public <R, P> R accept(AnnotationValueVisitor<R, P> v, @Nullable P p) {
    Object underlyingValue = underlyingAnnotationValue.getValue();
    Object value = getValue();

    if (underlyingValue.equals(value)) {
      return underlyingAnnotationValue.accept(v, p);
    }

    if (value instanceof TreeBackedVariableElement) {
      return v.visitEnumConstant((TreeBackedVariableElement) value, p);
    } else if (value instanceof TypeMirror) {
      return v.visitType((TypeMirror) value, p);
    } else if (value instanceof TreeBackedAnnotationMirror) {
      return v.visitAnnotation((TreeBackedAnnotationMirror) value, p);
    } else if (value instanceof List) {
      @SuppressWarnings("unchecked")
      List<? extends AnnotationValue> valuesList = (List<? extends AnnotationValue>) value;
      return v.visitArray(valuesList, p);
    } else {
      throw new IllegalStateException(String.format("Unexpected annotation value: %s", value));
    }
  }

  @Override
  public String toString() {
    return accept(
        new SimpleAnnotationValueVisitor8<String, Void>() {
          @Override
          protected String defaultAction(Object o, Void aVoid) {
            return o.toString();
          }

          @Override
          public String visitByte(byte b, Void aVoid) {
            return String.format("(byte)0x%x", b);
          }

          @Override
          public String visitChar(char c, Void aVoid) {
            return String.format("'%c'", c);
          }

          @Override
          public String visitString(String s, Void aVoid) {
            return String.format("\"%s\"", s);
          }

          @Override
          public String visitLong(long l, Void aVoid) {
            return String.format("%dL", l);
          }

          @Override
          public String visitFloat(float f, Void aVoid) {
            return String.format("%.1ff", f);
          }

          @Override
          public String visitEnumConstant(VariableElement c, Void aVoid) {
            return c.toString();
          }

          @Override
          public String visitType(TypeMirror t, Void aVoid) {
            return String.format("%s.class", t.toString());
          }

          @Override
          public String visitArray(List<? extends AnnotationValue> vals, Void aVoid) {
            return String.format(
                "{%s}",
                vals.stream().map(val -> val.accept(this, null)).collect(Collectors.joining(", ")));
          }
        },
        null);
  }
}
