/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java.abi.source;

import com.facebook.buck.jvm.java.abi.source.api.FrontendOnlyJavacTaskProxy;
import com.facebook.buck.jvm.java.plugin.adapter.BuckJavacTaskProxyImpl;
import com.sun.source.util.JavacTask;
import javax.tools.JavaCompiler;

public class FrontendOnlyJavacTaskProxyImpl extends BuckJavacTaskProxyImpl
    implements FrontendOnlyJavacTaskProxy {
  private final FrontendOnlyJavacTask javacTask;

  public FrontendOnlyJavacTaskProxyImpl(JavaCompiler.CompilationTask task) {
    this(new FrontendOnlyJavacTask((JavacTask) task));
  }

  public FrontendOnlyJavacTaskProxyImpl(FrontendOnlyJavacTask javacTask) {
    super(javacTask);
    this.javacTask = javacTask;
  }

  @Override
  public FrontendOnlyJavacTask getInner() {
    return javacTask;
  }
}
