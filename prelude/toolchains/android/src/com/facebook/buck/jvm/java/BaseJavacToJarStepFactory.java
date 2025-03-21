/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.jvm.core.BuildTargetValue;
import com.facebook.buck.step.isolatedsteps.IsolatedStep;
import com.facebook.buck.step.isolatedsteps.common.MkdirIsolatedStep;
import com.google.common.collect.ImmutableList;
import javax.annotation.Nullable;

/**
 * Java implementation of compile to jar steps factory that doesn't depend on internal build graph
 * datastructures, and only knows how to create compile steps.
 */
public class BaseJavacToJarStepFactory extends BaseCompileToJarStepFactory<JavaExtraParams> {
  public BaseJavacToJarStepFactory() {}

  @Override
  public JavaExtraParams castExtraParams(ExtraParams extraParams) {
    return (JavaExtraParams) extraParams;
  }

  @Override
  public void createCompileStep(
      RelPath buckOut,
      AbsPath buildCellRootPath,
      BuildTargetValue invokingRule,
      CompilerOutputPathsValue compilerOutputPathsValue,
      CompilerParameters parameters,
      ImmutableList.Builder<IsolatedStep> steps,
      ResolvedJavac resolvedJavac,
      @Nullable ActionMetadata actionMetadata,
      JavaExtraParams extraParams,
      RelPath kotlinClassesDir) {

    CompilerOutputPaths outputPath = compilerOutputPathsValue.getByType(invokingRule.getType());
    if (extraParams.getAddAnnotationPath()) {
      addAnnotationGenFolderStep(steps, outputPath.getAnnotationPath());
    }

    ResolvedJavacOptions resolvedJavacOptions = extraParams.getResolvedJavacOptions();
    steps.add(
        new JavacStep(
            resolvedJavac,
            resolvedJavacOptions,
            invokingRule,
            buckOut,
            compilerOutputPathsValue,
            parameters,
            null,
            null));
  }

  protected void addAnnotationGenFolderStep(
      ImmutableList.Builder<IsolatedStep> steps, RelPath annotationGenFolder) {
    steps.add(new MkdirIsolatedStep(annotationGenFolder));
  }
}
