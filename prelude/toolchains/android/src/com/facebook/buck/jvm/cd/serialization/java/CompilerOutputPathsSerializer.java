/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.cd.serialization.java;

import com.facebook.buck.cd.model.java.OutputPathsValue;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.jvm.cd.serialization.RelPathSerializer;
import com.facebook.buck.jvm.java.CompilerOutputPaths;
import java.util.Optional;

/** {@link CompilerOutputPaths} to protobuf serializer */
public class CompilerOutputPathsSerializer {

  private CompilerOutputPathsSerializer() {}

  /**
   * Serializes {@link CompilerOutputPaths} into javacd model's {@link
   * OutputPathsValue.OutputPaths}.
   */
  public static OutputPathsValue.OutputPaths serialize(CompilerOutputPaths compilerOutputPaths) {
    OutputPathsValue.OutputPaths.Builder builder = OutputPathsValue.OutputPaths.newBuilder();
    builder.setClassesDir(toRelPath(compilerOutputPaths.getClassesDir()));
    builder.setOutputJarDirPath(toRelPath(compilerOutputPaths.getOutputJarDirPath()));
    compilerOutputPaths
        .getAbiJarPath()
        .map(RelPathSerializer::serialize)
        .ifPresent(builder::setAbiJarPath);
    builder.setAnnotationPath(toRelPath(compilerOutputPaths.getAnnotationPath()));
    builder.setPathToSourcesList(toRelPath(compilerOutputPaths.getPathToSourcesList()));
    builder.setWorkingDirectory(toRelPath(compilerOutputPaths.getWorkingDirectory()));
    compilerOutputPaths
        .getOutputJarPath()
        .map(RelPathSerializer::serialize)
        .ifPresent(builder::setOutputJarPath);
    return builder.build();
  }

  private static String toRelPath(RelPath relPath) {
    return RelPathSerializer.serialize(relPath);
  }

  /**
   * Deserializes javacd model's {@link OutputPathsValue.OutputPaths} into {@link
   * CompilerOutputPaths}.
   */
  public static CompilerOutputPaths deserialize(OutputPathsValue.OutputPaths outputPaths) {
    return deserialize(outputPaths, Optional.empty());
  }

  public static CompilerOutputPaths deserialize(
      OutputPathsValue.OutputPaths outputPaths, Optional<RelPath> tmpDir) {
    return new CompilerOutputPaths(
        toRelPath(outputPaths.getClassesDir()),
        toRelPath(outputPaths.getOutputJarDirPath()),
        toOptionalRelPath(outputPaths.getAbiJarPath()),
        toRelPath(outputPaths.getAnnotationPath()),
        outputPaths.getPathToSourcesList().isEmpty()
            ? tmpDir.map(p -> p.resolveRel("__srcs__")).get()
            : toRelPath(outputPaths.getPathToSourcesList()),
        outputPaths.getWorkingDirectory().isEmpty()
            ? tmpDir.get()
            : toRelPath(outputPaths.getWorkingDirectory()),
        toOptionalRelPath(outputPaths.getOutputJarPath()));
  }

  private static RelPath toRelPath(String relPath) {
    return RelPathSerializer.deserialize(relPath);
  }

  private static Optional<RelPath> toOptionalRelPath(String value) {
    return Optional.of(value)
        .filter(s -> !s.isEmpty())
        .map(CompilerOutputPathsSerializer::toRelPath);
  }
}
