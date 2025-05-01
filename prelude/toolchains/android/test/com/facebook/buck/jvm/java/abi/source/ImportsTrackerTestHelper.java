/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java.abi.source;

import com.sun.source.tree.CompilationUnitTree;
import com.sun.source.tree.ExpressionTree;
import com.sun.source.tree.ImportTree;
import com.sun.source.tree.MemberSelectTree;
import com.sun.source.util.TreePath;
import com.sun.source.util.Trees;
import javax.lang.model.element.Name;
import javax.lang.model.element.PackageElement;
import javax.lang.model.element.QualifiedNameable;
import javax.lang.model.element.TypeElement;
import javax.lang.model.util.Elements;
import javax.lang.model.util.Types;

class ImportsTrackerTestHelper {

  public static ImportsTracker loadImports(
      Elements elements, Types types, Trees trees, CompilationUnitTree compilationUnit) {
    TreePath compilationUnitPath = new TreePath(compilationUnit);
    ImportsTracker result =
        new ImportsTracker(elements, types, (PackageElement) trees.getElement(compilationUnitPath));
    for (ImportTree importTree : compilationUnit.getImports()) {
      handleImport(trees, result, new TreePath(compilationUnitPath, importTree));
    }
    return result;
  }

  private static void handleImport(Trees trees, ImportsTracker imports, TreePath importTreePath) {
    ImportTree importTree = (ImportTree) importTreePath.getLeaf();
    MemberSelectTree importedExpression = (MemberSelectTree) importTree.getQualifiedIdentifier();
    TreePath importedExpressionPath = new TreePath(importTreePath, importedExpression);
    Name simpleName = importedExpression.getIdentifier();
    boolean isStarImport = simpleName.contentEquals("*");

    if (!isStarImport && !importTree.isStatic()) {
      TypeElement importedType = (TypeElement) trees.getElement(importedExpressionPath);
      imports.importType(importedType, importedExpressionPath);
    } else {
      ExpressionTree containingElementExpression = importedExpression.getExpression();
      TreePath containingElementExpressionPath =
          new TreePath(importedExpressionPath, containingElementExpression);
      QualifiedNameable containingElement =
          (QualifiedNameable) trees.getElement(containingElementExpressionPath);

      if (importTree.isStatic()) {
        TypeElement containingType = (TypeElement) containingElement;
        if (isStarImport) {
          imports.importStaticMembers((TypeElement) containingElement);
        } else {
          imports.importStatic(containingType, simpleName);
        }
      } else {
        // Normal star import
        imports.importMembers(containingElement, containingElementExpressionPath);
      }
    }
  }
}
