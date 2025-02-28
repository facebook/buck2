/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java.plugin.adapter;

import com.facebook.buck.jvm.java.plugin.api.CompilationUnitTreeProxy;
import com.sun.source.tree.CompilationUnitTree;

public class CompilationUnitTreeProxyImpl implements CompilationUnitTreeProxy {
  private final CompilationUnitTree compilationUnitTree;

  public CompilationUnitTreeProxyImpl(CompilationUnitTree compilationUnitTree) {
    this.compilationUnitTree = compilationUnitTree;
  }

  public CompilationUnitTree getCompilationUnit() {
    return compilationUnitTree;
  }
}
