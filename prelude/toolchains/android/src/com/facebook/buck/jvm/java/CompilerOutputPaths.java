/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java;

import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.core.util.immutables.BuckStyleValueWithBuilder;
import java.util.Optional;

/** Provides access to the various output paths for a java library. */
@BuckStyleValueWithBuilder
public abstract class CompilerOutputPaths {

  public abstract RelPath getClassesDir();

  public abstract RelPath getOutputJarDirPath();

  public abstract Optional<RelPath> getAbiJarPath();

  public abstract RelPath getAnnotationPath();

  public abstract RelPath getPathToSourcesList();

  public abstract RelPath getWorkingDirectory();

  public abstract Optional<RelPath> getOutputJarPath();

  /** Returns a path to a file that contains dependencies used in the compilation */
  public static RelPath getJavaDepFilePath(RelPath outputJarDirPath) {
    return outputJarDirPath.resolveRel("used-classes.json");
  }

  /** Returns a path to a file that contains all dependencies used in Kotlin compilation */
  public static RelPath getKotlinDepFilePath(RelPath outputJarDirPath) {
    return outputJarDirPath.resolveRel("kotlin-used-classes.json");
  }

  public static RelPath getJvmAbiGenFilePath(RelPath outputJarDirPath) {
    return outputJarDirPath.resolveRel("jvm-abi-gen.jar");
  }

  /** Returns a path to the temp file that contains dependencies directly used in Kotlin compiler */
  public static RelPath getKotlinTempDepFilePath(RelPath reportDirPath) {
    return reportDirPath.resolveRel("kotlin-used-classes-tmp.json");
  }

  /** Returns a path to a file that contains files accessed by KAPT in Kotlin compilation */
  public static RelPath getKAPTDepFilePath(RelPath reportDirPath) {
    return reportDirPath.resolveRel("kapt-used-classes-tmp.txt");
  }

  /** Returns a path to a file that contains files accessed by KSP in Kotlin compilation */
  public static RelPath getKspDepFilePath(RelPath reportDirPath) {
    return reportDirPath.resolveRel("ksp-used-classes-tmp.txt");
  }

  public static ImmutableCompilerOutputPaths.Builder builder() {
    return ImmutableCompilerOutputPaths.builder();
  }
}
