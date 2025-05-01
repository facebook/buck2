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
import com.sun.source.tree.AnnotationTree;
import com.sun.source.tree.ClassTree;
import com.sun.source.tree.CompilationUnitTree;
import com.sun.source.tree.Tree;
import com.sun.source.tree.Tree.Kind;
import com.sun.source.util.TreePath;
import com.sun.source.util.TreePathScanner;
import com.sun.source.util.Trees;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Queue;
import java.util.Set;
import java.util.stream.Collectors;
import javax.lang.model.element.AnnotationMirror;
import javax.lang.model.element.Element;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.PackageElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.TypeParameterElement;
import javax.lang.model.element.VariableElement;
import javax.lang.model.util.ElementScanner8;
import javax.tools.JavaFileObject;

/**
 * Creates {@link TreeBackedElement}s for each element in the {@link CompilationUnitTree}s known to
 * the compiler. This is analogous to the "Enter" phase of javac.
 */
class TreeBackedEnter {

  private final TreeBackedElements elements;
  private final TreeBackedTypes types;
  private final Trees javacTrees;
  // asdf
  private final EnteringElementScanner elementScanner = new EnteringElementScanner();
  private final PostEnterCanonicalizer canonicalizer;

  TreeBackedEnter(TreeBackedElements elements, TreeBackedTypes types, Trees javacTrees) {
    this.elements = elements;
    this.types = types;
    this.javacTrees = javacTrees;
    canonicalizer = new PostEnterCanonicalizer(elements, types, javacTrees);
  }

  public void enter(CompilationUnitTree compilationUnit) {
    elementScanner.enter(compilationUnit);
  }

  private class EnteringElementScanner extends ElementScanner8<Void, Void> {

    private final Deque<TreeBackedElement> contextStack = new ArrayDeque<>();
    @Nullable private TreePath currentPath;
    @Nullable private Tree currentTree;
    @Nullable private ElementTreeFinder elementTreeFinder;
    @Nullable private TreeBackedPackageElement currentPackage;

    private TreeBackedElement getCurrentContext() {
      return contextStack.peek();
    }

    private TreePath getCurrentPath() {
      return Objects.requireNonNull(currentPath);
    }

    public void enter(CompilationUnitTree compilationUnitTree) {
      new TreePathScanner<Void, Void>() {
        @Override
        public Void visitCompilationUnit(CompilationUnitTree node, Void aVoid) {
          elementTreeFinder = ElementTreeFinder.forCompilationUnit(compilationUnitTree, javacTrees);
          currentPath = getCurrentPath();
          currentTree = currentPath.getLeaf();
          try {
            return super.visitCompilationUnit(node, aVoid);
          } finally {
            exitPackageContextIfNecessary();
            currentPath = null;
            currentTree = null;
          }
        }

        @Override
        public Void visitAnnotation(AnnotationTree node, Void aVoid) {
          enterPackageContextIfNecessary();
          return null;
        }

        @Override
        public Void visitClass(ClassTree node, Void aVoid) {
          enterPackageContextIfNecessary();

          // We use the Tree to find the top-level type elements in a given compilation unit,
          // then switch to Element scanning so that we can catch elements created by the compiler
          // that don't have a tree, such as default constructors or the generated methods on enums.
          TreePath previousPath = currentPath;
          Tree previousTree = currentTree;
          currentPath = getCurrentPath();
          currentTree = currentPath.getLeaf();
          try {
            Element element = javacTrees.getElement(currentPath);
            if (element != null) {
              EnteringElementScanner.this.scan(element, null);
            } else if (node.getKind() != Tree.Kind.EMPTY_STATEMENT) {
              throw new AssertionError(String.format("Unexpected tree kind %s", node.getKind()));
            }
          } finally {
            currentPath = previousPath;
            currentTree = previousTree;
          }
          return null;
        }

        private void enterPackageContextIfNecessary() {
          if (currentPackage == null) {
            currentPackage = enterPackageElement();
            contextStack.push(currentPackage);
          }
        }

        private void exitPackageContextIfNecessary() {
          if (currentPackage != null) {
            contextStack.pop();
            currentPackage = null;
          }
        }
      }.scan(compilationUnitTree, null);
    }

    private TreeBackedPackageElement enterPackageElement() {
      CompilationUnitTree compilationUnitTree = getCurrentPath().getCompilationUnit();
      PackageElement packageElement =
          (PackageElement) Objects.requireNonNull(javacTrees.getElement(getCurrentPath()));
      TreeBackedPackageElement treeBackedPackageElement =
          elements.enterElement(packageElement, this::newTreeBackedPackage);
      if (compilationUnitTree
          .getSourceFile()
          .isNameCompatible("package-info", JavaFileObject.Kind.SOURCE)) {
        treeBackedPackageElement.setTreePath(getCurrentPath());
        enterAnnotationMirrors(treeBackedPackageElement);
      }
      return treeBackedPackageElement;
    }

    @Override
    public Void scan(Element e, @Nullable Void aVoid) {
      TreePath previousPath = currentPath;
      Tree previousTree = currentTree;
      currentTree = Objects.requireNonNull(elementTreeFinder).getTree(e);
      currentPath = currentTree == null ? null : new TreePath(currentPath, currentTree);
      try {
        if (currentPath != null && javacTrees.getElement(currentPath) != e) {
          throw new AssertionError(
              String.format(
                  "Element mismatch!\n  Expected: %s\n  Found: %s\n",
                  e, javacTrees.getElement(currentPath)));
        }
        return super.scan(e, aVoid);
      } finally {
        currentPath = previousPath;
        currentTree = previousTree;
      }
    }

    @Override
    public Void visitType(TypeElement e, Void v) {
      TreeBackedTypeElement newClass = elements.enterElement(e, this::newTreeBackedType);
      try (ElementContext c = new ElementContext(newClass)) {
        scan(reallyGetEnclosedElements(e, getCurrentPath()), v);
        scan(e.getTypeParameters(), v);
        return null;
      }
    }

    /**
     * When some method parameters might be ErrorTypes, javac will ignore overloads when entering
     * elements. On the flip side, there are no trees for generated elements like default
     * constructors. To make sure we find all the elements, we look at both sources and merge the
     * lists.
     */
    private List<? extends Element> reallyGetEnclosedElements(TypeElement e, TreePath path) {
      // fromElement contains the elements that the compiler found or generated. Because of
      // the weird ErrorType behavior, this will be missing elements that the user wrote.
      List<? extends Element> fromElement = e.getEnclosedElements();
      Queue<? extends Element> fromElementQueue = new ArrayDeque<>(fromElement);
      Set<? extends Element> fromElementSet = new HashSet<>(fromElement);

      // fromTree contains the elements that the user wrote. It will be missing compiler-generated
      // elements.
      List<? extends Element> fromTree =
          ((ClassTree) path.getLeaf())
              .getMembers().stream()
                  // Top level blocks (static initializers and anonymous blocks) are only used at
                  // runtime, so we can safely skip them for ABI generation purposes. In Java 9 and
                  // later, it's actually necessary to skip them, as getElement will attempt to
                  // attribute the parent class node when given block nodes, effectively doing an
                  // analyze compiler phase, which we need to avoid. In Java 8 and earlier,
                  // getElement
                  // just returns null in these cases.
                  .filter(tree -> tree.getKind() != Kind.BLOCK)
                  .map(tree -> javacTrees.getElement(new TreePath(path, tree)))
                  .collect(Collectors.toList());
      Set<? extends Element> fromTreeSet = new HashSet<>(fromTree);

      List<Element> result = new ArrayList<>();
      for (Element elementFromTree : fromTree) {
        if (fromElementSet.contains(elementFromTree)) {
          // Output any compiler-generated elements that come before this one in the compiler's
          // element list.
          Element elementFromElement = fromElementQueue.poll();
          while (elementFromElement != null && !fromTreeSet.contains(elementFromElement)) {
            result.add(elementFromElement);
            elementFromElement = fromElementQueue.poll();
          }
        }
        result.add(elementFromTree);
      }

      return result;
    }

    @Override
    public Void visitTypeParameter(TypeParameterElement e, Void v) {
      TreeBackedTypeParameterElement typeParameter =
          elements.enterElement(e, this::newTreeBackedTypeParameter);

      try (ElementContext c = new ElementContext(typeParameter)) {
        return super.visitTypeParameter(e, v);
      }
    }

    @Override
    public Void visitExecutable(ExecutableElement e, Void v) {
      TreeBackedExecutableElement method = elements.enterElement(e, this::newTreeBackedExecutable);

      try (ElementContext c = new ElementContext(method)) {
        super.visitExecutable(e, v);
        scan(e.getTypeParameters(), v);
        return null;
      }
    }

    @Override
    public Void visitVariable(VariableElement e, Void v) {
      elements.enterElement(e, this::newTreeBackedVariable);
      return super.visitVariable(e, v);
    }

    private TreeBackedPackageElement newTreeBackedPackage(PackageElement underlyingPackage) {
      return new TreeBackedPackageElement(underlyingPackage);
    }

    private TreeBackedTypeElement newTreeBackedType(TypeElement underlyingType) {
      TreeBackedTypeElement typeElement =
          new TreeBackedTypeElement(
              types, underlyingType, getCurrentContext(), getCurrentPath(), canonicalizer);
      enterAnnotationMirrors(typeElement);
      return typeElement;
    }

    private TreeBackedTypeParameterElement newTreeBackedTypeParameter(
        TypeParameterElement underlyingTypeParameter) {
      TreeBackedParameterizable enclosingElement = (TreeBackedParameterizable) getCurrentContext();

      // TreeBackedExecutables with a null tree occur only for compiler-generated methods such
      // as default construvtors. Those never have type parameters, so we should never find
      // ourselves here without a tree.
      TreeBackedTypeParameterElement result =
          new TreeBackedTypeParameterElement(
              types, underlyingTypeParameter, getCurrentPath(), enclosingElement, canonicalizer);
      enterAnnotationMirrors(result);

      enclosingElement.addTypeParameter(result);
      return result;
    }

    private TreeBackedExecutableElement newTreeBackedExecutable(
        ExecutableElement underlyingExecutable) {
      TreeBackedExecutableElement result =
          new TreeBackedExecutableElement(
              underlyingExecutable, getCurrentContext(), currentPath, canonicalizer);
      enterAnnotationMirrors(result);
      return result;
    }

    private TreeBackedVariableElement newTreeBackedVariable(VariableElement underlyingVariable) {
      TreeBackedElement enclosingElement = getCurrentContext();
      TreeBackedVariableElement result =
          new TreeBackedVariableElement(
              underlyingVariable, enclosingElement, currentPath, canonicalizer);
      enterAnnotationMirrors(result);
      return result;
    }

    private void enterAnnotationMirrors(TreeBackedElement element) {
      List<? extends AnnotationMirror> underlyingAnnotations =
          element.getUnderlyingElement().getAnnotationMirrors();
      if (underlyingAnnotations.isEmpty()) {
        return;
      }

      for (var underlyingAnnotation : underlyingAnnotations) {
        var annotationTreePath =
            javacTrees.getPath(element.getUnderlyingElement(), underlyingAnnotation);
        if (annotationTreePath == null) {
          throw new IllegalArgumentException(
              String.format("Annotation node of element %s could not be found.", element));
        }
        element.addAnnotationMirror(
            new TreeBackedAnnotationMirror(
                underlyingAnnotation, annotationTreePath, canonicalizer));
      }
    }

    private class ElementContext implements AutoCloseable {

      public ElementContext(TreeBackedElement newContext) {
        contextStack.push(newContext);
      }

      @Override
      public void close() {
        contextStack.pop();
      }
    }
  }
}
