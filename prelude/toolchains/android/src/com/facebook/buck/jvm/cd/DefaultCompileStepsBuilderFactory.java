/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.cd;

import com.facebook.buck.jvm.java.CompileToJarStepFactory;

/**
 * Factory that creates {@link CompileStepsBuilder } builders instances that returns steps that is
 * ready to be executed in the current process.
 */
public class DefaultCompileStepsBuilderFactory<T extends CompileToJarStepFactory.ExtraParams>
    implements CompileStepsBuilderFactory {

  private final CompileToJarStepFactory<T> configuredCompiler;

  public DefaultCompileStepsBuilderFactory(CompileToJarStepFactory<T> configuredCompiler) {
    this.configuredCompiler = configuredCompiler;
  }

  /** Creates an appropriate {@link LibraryStepsBuilder} instance */
  @Override
  public LibraryStepsBuilder getLibraryBuilder() {
    return new DefaultLibraryStepsBuilder<>(configuredCompiler);
  }

  /** Creates an appropriate {@link AbiStepsBuilder} instance */
  @Override
  public AbiStepsBuilder getAbiBuilder() {
    return new DefaultAbiStepsBuilder<>(configuredCompiler);
  }
}
