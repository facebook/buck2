/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.cd;

import com.facebook.buck.cd.model.java.AbiGenerationMode;
import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.jvm.core.BuildTargetValue;
import com.facebook.buck.jvm.java.ActionMetadata;
import com.facebook.buck.jvm.java.CompileToJarStepFactory;
import com.facebook.buck.jvm.java.CompilerOutputPathsValue;
import com.facebook.buck.jvm.java.CompilerParameters;
import com.facebook.buck.jvm.java.JarParameters;
import com.facebook.buck.jvm.java.ResolvedJavac;
import com.facebook.buck.step.isolatedsteps.common.MakeCleanDirectoryIsolatedStep;
import com.facebook.buck.step.isolatedsteps.java.MakeMissingOutputsStep;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSortedSet;
import javax.annotation.Nullable;

/** Default implementation of {@link LibraryStepsBuilder} */
class DefaultLibraryStepsBuilder<T extends CompileToJarStepFactory.ExtraParams>
    extends DefaultCompileStepsBuilderBase<T> implements LibraryStepsBuilder {

  DefaultLibraryStepsBuilder(CompileToJarStepFactory<T> configuredCompiler) {
    super(configuredCompiler);
  }

  @Override
  public void addMakeMissingOutputsStep(RelPath annotationsPath) {
    stepsBuilder.add(new MakeMissingOutputsStep(annotationsPath));
  }

  @Override
  public void addBuildStepsForLibrary(
      AbiGenerationMode abiCompatibilityMode,
      AbiGenerationMode abiGenerationMode,
      boolean isRequiredForSourceOnlyAbi,
      boolean trackClassUsage,
      RelPath buckOut,
      BuildTargetValue buildTargetValue,
      CompilerOutputPathsValue compilerOutputPathsValue,
      ImmutableList<RelPath> compileTimeClasspathPaths,
      ImmutableMap<RelPath, RelPath> compileTimeClasspathSnapshotPaths,
      ImmutableSortedSet<RelPath> javaSrcs,
      ImmutableMap<RelPath, RelPath> resourcesMap,
      @Nullable JarParameters libraryJarParameters,
      AbsPath buildCellRootPath,
      ResolvedJavac resolvedJavac,
      @Nullable ActionMetadata actionMetadata,
      CompileToJarStepFactory.ExtraParams extraParams,
      @Nullable RelPath kotlinClassesDir) {

    CompilerParameters compilerParameters =
        JavaLibraryRules.getCompilerParameters(
            compileTimeClasspathPaths,
            compileTimeClasspathSnapshotPaths,
            javaSrcs,
            buildTargetValue.getFullyQualifiedName(),
            trackClassUsage,
            abiGenerationMode,
            abiCompatibilityMode,
            isRequiredForSourceOnlyAbi,
            compilerOutputPathsValue.getByType(buildTargetValue.getType()));

    stepsBuilder.addAll(
        MakeCleanDirectoryIsolatedStep.of(
            compilerOutputPathsValue.getByType(buildTargetValue.getType()).getWorkingDirectory()));

    configuredCompiler.createCompileToJarStep(
        buckOut,
        buildCellRootPath,
        buildTargetValue,
        compilerOutputPathsValue,
        compilerParameters,
        null,
        libraryJarParameters,
        stepsBuilder,
        resourcesMap,
        resolvedJavac,
        actionMetadata,
        configuredCompiler.castExtraParams(extraParams),
        kotlinClassesDir);
  }
}
