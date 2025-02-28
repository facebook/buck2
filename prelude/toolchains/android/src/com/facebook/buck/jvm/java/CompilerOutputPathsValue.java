/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java;

import com.facebook.buck.cd.model.java.BuildTargetValue.Type;
import com.facebook.buck.core.util.immutables.BuckStyleValue;

/**
 * Value object that contains {@link CompilerOutputPaths} for library, source abi and source only
 * abi targets as well as library target fully qualified name.
 */
@BuckStyleValue
public abstract class CompilerOutputPathsValue {

  public abstract String getLibraryTargetFullyQualifiedName();

  public abstract CompilerOutputPaths getLibraryCompilerOutputPath();

  public abstract CompilerOutputPaths getSourceAbiCompilerOutputPath();

  public abstract CompilerOutputPaths getSourceOnlyAbiCompilerOutputPath();

  /** Returns {@link CompilerOutputPaths} by given {@code type} */
  public CompilerOutputPaths getByType(Type type) {
    switch (type) {
      case LIBRARY:
        return getLibraryCompilerOutputPath();

      case SOURCE_ABI:
        return getSourceAbiCompilerOutputPath();

      case SOURCE_ONLY_ABI:
        return getSourceOnlyAbiCompilerOutputPath();

      case UNKNOWN:
      case UNRECOGNIZED:
      default:
        throw new IllegalStateException(type + " is not supported");
    }
  }

  /** Creates {@link CompilerOutputPathsValue} */
  public static CompilerOutputPathsValue of(
      String libraryTargetFullyQualifiedName,
      CompilerOutputPaths libraryCompilerOutputPath,
      CompilerOutputPaths sourceAbiCompilerOutputPath,
      CompilerOutputPaths sourceOnlyAbiCompilerOutputPath) {
    return ImmutableCompilerOutputPathsValue.ofImpl(
        libraryTargetFullyQualifiedName,
        libraryCompilerOutputPath,
        sourceAbiCompilerOutputPath,
        sourceOnlyAbiCompilerOutputPath);
  }
}
