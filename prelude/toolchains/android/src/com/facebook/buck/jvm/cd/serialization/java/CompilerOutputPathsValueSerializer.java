/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.cd.serialization.java;

import com.facebook.buck.cd.model.java.OutputPathsValue;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.jvm.java.CompilerOutputPaths;
import com.facebook.buck.jvm.java.CompilerOutputPathsValue;
import java.util.Optional;

/** {@link CompilerOutputPathsValue} to protobuf serializer */
public class CompilerOutputPathsValueSerializer {

  private CompilerOutputPathsValueSerializer() {}

  /** Serializes {@link CompilerOutputPathsValue} into javacd model's {@link OutputPathsValue}. */
  public static OutputPathsValue serialize(CompilerOutputPathsValue value) {
    OutputPathsValue.Builder builder = OutputPathsValue.newBuilder();
    builder.setLibraryPaths(toOutputPaths(value.getLibraryCompilerOutputPath()));
    builder.setSourceAbiPaths(toOutputPaths(value.getSourceAbiCompilerOutputPath()));
    builder.setSourceOnlyAbiPaths(toOutputPaths(value.getSourceOnlyAbiCompilerOutputPath()));
    builder.setLibraryTargetFullyQualifiedName(value.getLibraryTargetFullyQualifiedName());
    return builder.build();
  }

  private static OutputPathsValue.OutputPaths toOutputPaths(CompilerOutputPaths outputPaths) {
    return CompilerOutputPathsSerializer.serialize(outputPaths);
  }

  /** Deserializes javacd model's {@link OutputPathsValue} into {@link CompilerOutputPathsValue}. */
  public static CompilerOutputPathsValue deserialize(OutputPathsValue outputPathsValue) {
    return deserialize(outputPathsValue, Optional.empty());
  }

  public static CompilerOutputPathsValue deserialize(
      OutputPathsValue outputPathsValue, Optional<RelPath> tmpDir) {
    return CompilerOutputPathsValue.of(
        outputPathsValue.getLibraryTargetFullyQualifiedName(),
        toCompilerOutputPaths(outputPathsValue.getLibraryPaths(), tmpDir),
        toCompilerOutputPaths(outputPathsValue.getSourceAbiPaths(), tmpDir),
        toCompilerOutputPaths(outputPathsValue.getSourceOnlyAbiPaths(), tmpDir));
  }

  private static CompilerOutputPaths toCompilerOutputPaths(
      OutputPathsValue.OutputPaths outputPaths, Optional<RelPath> tmpDir) {
    return CompilerOutputPathsSerializer.deserialize(outputPaths, tmpDir);
  }
}
