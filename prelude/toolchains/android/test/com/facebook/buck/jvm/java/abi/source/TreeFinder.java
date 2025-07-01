/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.java.abi.source;

import com.sun.source.tree.BlockTree;
import com.sun.source.tree.ClassTree;
import com.sun.source.tree.CompilationUnitTree;
import com.sun.source.tree.MethodTree;
import com.sun.source.tree.Tree;
import com.sun.source.tree.TypeParameterTree;
import com.sun.source.tree.VariableTree;
import com.sun.source.util.TreeScanner;

public class TreeFinder {
  public static Tree findTreeNamed(CompilationUnitTree compilationUnit, CharSequence name) {
    return new TreeScanner<Tree, Void>() {
      @Override
      public Tree visitClass(ClassTree node, Void aVoid) {
        if (node.getSimpleName().contentEquals(name)) {
          return node;
        }

        return super.visitClass(node, aVoid);
      }

      @Override
      public Tree visitMethod(MethodTree node, Void aVoid) {
        if (node.getName().contentEquals(name)) {
          return node;
        }

        return super.visitMethod(node, aVoid);
      }

      @Override
      public Tree visitVariable(VariableTree node, Void aVoid) {
        if (node.getName().contentEquals(name)) {
          return node;
        }

        return null;
      }

      @Override
      public Tree visitTypeParameter(TypeParameterTree node, Void aVoid) {
        if (node.getName().contentEquals(name)) {
          return node;
        }

        return null;
      }

      @Override
      public Tree visitBlock(BlockTree node, Void aVoid) {
        return null;
      }

      @Override
      public Tree reduce(Tree r1, Tree r2) {
        if (r1 == r2) {
          return r1;
        }
        if (r1 != null && r2 != null) {
          throw new AssertionError();
        } else if (r1 != null) {
          return r1;
        } else {
          return r2;
        }
      }
    }.scan(compilationUnit, null);
  }
}
