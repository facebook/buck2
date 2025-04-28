/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java.abi;

import com.facebook.buck.jvm.java.abi.source.api.CannotInferException;
import com.facebook.buck.jvm.java.lang.model.AnnotationValueScanner8;
import com.facebook.buck.jvm.java.lang.model.ElementsExtended;
import com.google.common.collect.Lists;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import javax.annotation.Nullable;
import javax.lang.model.element.AnnotationMirror;
import javax.lang.model.element.AnnotationValue;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.NestingKind;
import javax.lang.model.element.PackageElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.TypeParameterElement;
import javax.lang.model.element.VariableElement;
import javax.lang.model.type.DeclaredType;
import javax.lang.model.type.TypeMirror;
import javax.lang.model.util.ElementScanner8;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.tree.InnerClassNode;

/**
 * Aids in constructing a table of {@link InnerClassNode}s when generating bytecode for a {@link
 * TypeElement}.
 */
public class InnerClassesTable {
  private final DescriptorFactory descriptorFactory;
  private final AccessFlags accessFlagsUtils;
  private final ElementsExtended elements;
  private final Element topElement;
  private final Set<TypeElement> referencesToInners = new HashSet<>();

  public InnerClassesTable(
      DescriptorFactory descriptorFactory,
      AccessFlags accessFlagsUtils,
      ElementsExtended elements,
      Element topElement) {
    this.descriptorFactory = descriptorFactory;
    this.accessFlagsUtils = accessFlagsUtils;
    this.elements = elements;
    this.topElement = topElement;
  }

  public void addTypeReferences(Element element) {
    new ElementScanner8<Void, Void>() {
      @Override
      public Void visitType(TypeElement e, Void aVoid) {
        if (e != element) {
          return null;
        }

        addTypeReferences(e.getAnnotationMirrors());
        addTypeReferences(elements.getAllTypeAnnotations(e));
        e.getTypeParameters().forEach(typeParam -> scan(typeParam, aVoid));
        addTypeReferences(e.getSuperclass());
        e.getInterfaces().forEach(InnerClassesTable.this::addTypeReferences);
        // Members will be visited in the call to super, below

        return super.visitType(e, aVoid);
      }

      @Override
      public Void visitExecutable(ExecutableElement e, Void aVoid) {
        addTypeReferences(e.getAnnotationMirrors());
        addTypeReferences(elements.getAllTypeAnnotations(e));
        e.getTypeParameters().forEach(typeParam -> scan(typeParam, aVoid));
        addTypeReferences(e.getReturnType());
        addTypeReferences(e.getDefaultValue());
        // Parameters will be visited in the call to super, below
        e.getThrownTypes().forEach(InnerClassesTable.this::addTypeReferences);
        return super.visitExecutable(e, aVoid);
      }

      @Override
      public Void visitVariable(VariableElement e, Void aVoid) {
        addTypeReferences(e.getAnnotationMirrors());
        addTypeReferences(elements.getAllTypeAnnotations(e));
        addTypeReferences(e.asType());
        return super.visitVariable(e, aVoid);
      }

      @Override
      public Void visitTypeParameter(TypeParameterElement e, Void aVoid) {
        addTypeReferences(e.getAnnotationMirrors());
        addTypeReferences(elements.getAllTypeAnnotations(e));
        addTypeReferences(e.asType());
        return super.visitTypeParameter(e, aVoid);
      }
    }.scan(element);
  }

  public void addTypeReferences(TypeMirror type) {
    new TypeScanner8<Void, Void>() {
      @Override
      public Void scan(@Nullable TypeMirror t, Void aVoid) {
        if (t == null) {
          return null;
        }
        return super.scan(t, aVoid);
      }

      @Override
      public Void visitDeclared(DeclaredType t, Void aVoid) {
        TypeElement element = (TypeElement) t.asElement();
        if (element.getNestingKind() == NestingKind.MEMBER) {
          referencesToInners.add(element);
          element.getEnclosingElement().asType().accept(this, null);
        }

        try {
          return super.visitDeclared(t, aVoid);
        } catch (CannotInferException e) {
          // We cannot know the enclosing type or type arguments of an inferred type, so
          // we can't visit them for type references. However, inferred type mirrors are
          // only returned from asType on an inferred type element. If there were
          // type arguments in the code, we'd have a StandaloneDeclaredType instead of
          // an InferredType. Similarly, the enclosing type will be visited by way of
          // visiting the enclosing element above. So it's safe to just ignore the
          // exception.

          return null;
        }
      }
    }.scan(type);
  }

  public void addTypeReferences(List<? extends AnnotationMirror> annotationMirrors) {
    annotationMirrors.forEach(this::addTypeReferences);
  }

  private void addTypeReferences(AnnotationMirror annotationMirror) {
    addTypeReferences(annotationMirror.getAnnotationType());
    annotationMirror.getElementValues().values().forEach(this::addTypeReferences);
  }

  private void addTypeReferences(@Nullable AnnotationValue annotationValue) {
    if (annotationValue == null) {
      return;
    }
    new AnnotationValueScanner8<Void, Void>() {
      @Override
      public Void visitType(TypeMirror t, Void aVoid) {
        addTypeReferences(t);
        return super.visitType(t, aVoid);
      }

      @Override
      public Void visitEnumConstant(VariableElement c, Void aVoid) {
        addTypeReferences(c.asType());
        return super.visitEnumConstant(c, aVoid);
      }

      @Override
      public Void visitAnnotation(AnnotationMirror a, Void aVoid) {
        addTypeReferences(a.getAnnotationType());
        return super.visitAnnotation(a, aVoid);
      }
    }.scan(annotationValue);
  }

  public void reportInnerClassReferences(ClassVisitor visitor) {
    List<TypeElement> enclosingClasses = new ArrayList<>();
    List<TypeElement> memberClasses = new ArrayList<>();

    ElementKind elementKind = topElement.getKind();
    if (elementKind.isClass() || elementKind.isInterface()) {
      TypeElement walker = (TypeElement) topElement;
      while (walker.getNestingKind() == NestingKind.MEMBER) {
        enclosingClasses.add(walker);
        walker = (TypeElement) walker.getEnclosingElement();
      }
    }

    ElementScanner8<Void, Void> elementScanner =
        new ElementScanner8<>() {
          @Override
          public Void visitPackage(PackageElement e, Void aVoid) {
            addTypeReferences(e.getAnnotationMirrors());

            // If we're being asked to report inner class references of a package, it really means
            // we're being asked to report inner class references of package-info.java; i.e., just
            // the package annotations. We therefore return without chaining to super to avoid
            // recursing into enclosed elements.
            return null;
          }

          @Override
          public Void visitType(TypeElement e, Void aVoid) {
            if (e != topElement && !memberClasses.contains(e)) {
              memberClasses.add(e);
              return null;
            }

            addTypeReferences(e);

            return super.visitType(e, aVoid);
          }
        };
    elementScanner.scan(topElement);

    Set<TypeElement> reported = new HashSet<>();
    for (TypeElement element : Lists.reverse(enclosingClasses)) {
      if (reported.add(element)) {
        visitor.visitInnerClass(
            descriptorFactory.getInternalName(element),
            descriptorFactory.getInternalName((TypeElement) element.getEnclosingElement()),
            element.getSimpleName().toString(),
            accessFlagsUtils.getAccessFlags(element) & ~Opcodes.ACC_SUPER);
      }
    }

    for (TypeElement element : Lists.reverse(memberClasses)) {
      if (reported.add(element)) {
        visitor.visitInnerClass(
            descriptorFactory.getInternalName(element),
            descriptorFactory.getInternalName((TypeElement) element.getEnclosingElement()),
            element.getSimpleName().toString(),
            accessFlagsUtils.getAccessFlags(element) & ~Opcodes.ACC_SUPER);
      }
    }

    referencesToInners.stream()
        .filter(reported::add)
        .sorted(Comparator.comparing(e -> e.getQualifiedName().toString()))
        .forEach(
            element ->
                visitor.visitInnerClass(
                    descriptorFactory.getInternalName(element),
                    descriptorFactory.getInternalName((TypeElement) element.getEnclosingElement()),
                    element.getSimpleName().toString(),
                    accessFlagsUtils.getAccessFlags(element) & ~Opcodes.ACC_SUPER));
  }
}
