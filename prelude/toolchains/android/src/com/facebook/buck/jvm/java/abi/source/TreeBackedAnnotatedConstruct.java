/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java.abi.source;

import com.facebook.buck.jvm.java.lang.model.AnnotationValueScanner8;
import com.facebook.buck.util.liteinfersupport.Nullable;
import java.lang.annotation.Annotation;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import javax.lang.model.AnnotatedConstruct;
import javax.lang.model.type.TypeMirror;

/**
 * An implementation of {@link javax.lang.model.AnnotatedConstruct} that uses only the information
 * available from a {@link com.sun.source.tree.Tree}. This results in an incomplete implementation;
 * see documentation for individual methods and {@link com.facebook.buck.jvm.java.abi.source} for
 * more information.
 */
public class TreeBackedAnnotatedConstruct implements ArtificialAnnotatedConstruct {
  private final AnnotatedConstruct underlyingConstruct;
  private final List<TreeBackedAnnotationMirror> annotationMirrors = new ArrayList<>();

  public TreeBackedAnnotatedConstruct(AnnotatedConstruct underlyingConstruct) {
    this.underlyingConstruct = underlyingConstruct;
  }

  /* package */ void addAnnotationMirror(TreeBackedAnnotationMirror annotationMirror) {
    annotationMirrors.add(annotationMirror);
  }

  @Override
  public List<? extends ArtificialAnnotationMirror> getAnnotationMirrors() {
    return Collections.unmodifiableList(annotationMirrors);
  }

  @Override
  public <A extends Annotation> A getAnnotation(Class<A> annotationType) {
    return TreeBackedAnnotationFactory.createOrGetUnderlying(
        this, underlyingConstruct, annotationType);
  }

  @Override
  public <A extends Annotation> A[] getAnnotationsByType(Class<A> annotationType) {
    return underlyingConstruct.getAnnotationsByType(annotationType);
  }

  /**
   * Attempt to call {@link AnnotatedConstruct#getAnnotation(Class)} on the underlying construct,
   * but throw a more useful exception if it throws.
   */
  /* package */ <A extends Annotation> A getAnnotationWithBetterErrors(Class<A> annotationType) {
    try {
      return underlyingConstruct.getAnnotation(annotationType);
    } catch (RuntimeException e) {
      Set<String> problematicTypes = new LinkedHashSet<>();
      for (TreeBackedAnnotationMirror mirror : annotationMirrors) {
        if (!mirror
            .getAnnotationType()
            .asElement()
            .getSimpleName()
            .contentEquals(annotationType.getSimpleName())) {
          continue;
        }
        for (TreeBackedAnnotationValue value : mirror.getElementValues().values()) {
          new ArgumentCheckingValueVisitor().scan(value, problematicTypes);
        }
      }
      throw new RuntimeException(
          String.format(
              "Exception when trying to get annotation %s from element %s.  "
                  + "These types are not available: %s",
              annotationType.getSimpleName(), this, problematicTypes),
          e);
    }
  }

  private static class ArgumentCheckingValueVisitor
      extends AnnotationValueScanner8<Void, Set<String>> {
    @Nullable
    @Override
    public Void visitType(TypeMirror t, Set<String> problems) {
      if (t instanceof StandaloneDeclaredType
          && ((StandaloneDeclaredType) t).asElement() instanceof InferredTypeElement) {
        problems.add(t.toString());
      }
      return super.visitType(t, problems);
    }
  }
}
