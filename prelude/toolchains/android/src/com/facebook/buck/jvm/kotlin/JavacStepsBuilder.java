/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.kotlin;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.io.file.FileExtensionMatcher;
import com.facebook.buck.io.file.PathMatcher;
import com.facebook.buck.jvm.core.BuildTargetValue;
import com.facebook.buck.jvm.java.BaseJavacToJarStepFactory;
import com.facebook.buck.jvm.java.CompilerOutputPathsValue;
import com.facebook.buck.jvm.java.CompilerParameters;
import com.facebook.buck.jvm.java.JavaExtraParams;
import com.facebook.buck.jvm.java.ResolvedJavac;
import com.facebook.buck.jvm.java.ResolvedJavacOptions;
import com.facebook.buck.step.isolatedsteps.IsolatedStep;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSortedSet;

public class JavacStepsBuilder {
  private static final PathMatcher KOTLIN_PATH_MATCHER = FileExtensionMatcher.of("kt");

  /**
   * Prepares the Java compilation step for any Java files left to compile in this rule. This also
   * compiles any Java files generated from annotation processors using KAPT and KSP.
   */
  public static void prepareJavaCompilationIfNeeded(
      BuildTargetValue invokingRule,
      AbsPath buildCellRootPath,
      ImmutableList.Builder<IsolatedStep> steps,
      RelPath buckOut,
      CompilerOutputPathsValue compilerOutputPathsValue,
      CompilerParameters parameters,
      ResolvedJavac resolvedJavac,
      ResolvedJavacOptions resolvedJavacOptions,
      ImmutableList<RelPath> declaredClasspathEntries,
      ImmutableList<AbsPath> extraClassPaths,
      RelPath outputDirectory,
      ImmutableSortedSet.Builder<RelPath> sourceBuilder) {

    // Kotlin source-only-abi is only available for pure-kotlin targets
    // It's not applicable for:
    // - Java targets
    // - Mixed targets
    //
    // Buck doesn't check if it runs source-only-abi for non-pure-kotlin targets,
    // source-only-abi applicability for target should be verified externally.
    //
    // source-only-abi.jar packing happens via [KotlincToJarStepFactory::createCompileToJarStepImpl]
    if (invokingRule.isSourceOnlyAbi()) {
      return;
    }

    // Note that this filters out only .kt files, so this keeps both .java and .src.zip files.
    ImmutableSortedSet<RelPath> javaSourceFiles =
        sourceBuilder.build().stream()
            .filter(input -> !KOTLIN_PATH_MATCHER.matches(input))
            .collect(ImmutableSortedSet.toImmutableSortedSet(RelPath.comparator()));

    // No point running javac if there is no source file
    if (javaSourceFiles.isEmpty()) {
      return;
    }

    CompilerParameters javacParameters =
        new CompilerParameters(
            javaSourceFiles,
            ImmutableSortedSet.orderedBy(RelPath.comparator())
                .add(outputDirectory)
                .addAll(extraClassPaths.stream().map(buildCellRootPath::relativize).iterator())
                .addAll(declaredClasspathEntries)
                .build()
                .asList(),
            parameters.getClasspathSnapshots(),
            parameters.getOutputPaths(),
            parameters.getAbiGenerationMode(),
            parameters.getAbiCompatibilityMode(),
            parameters.getShouldTrackClassUsage(),
            parameters.getSourceOnlyAbiRuleInfoFactory());

    // Indicate no annotation processing required from this factory.  It is already handled by the
    // Kotlin factory, when it resolves javac's options.
    BaseJavacToJarStepFactory javacToJarStepFactory = new BaseJavacToJarStepFactory();

    javacToJarStepFactory.createCompileStep(
        buckOut,
        buildCellRootPath,
        invokingRule,
        compilerOutputPathsValue,
        javacParameters,
        steps,
        resolvedJavac,
        null,
        JavaExtraParams.of(resolvedJavacOptions, /* addAnnotationPath */ false),
        null);
  }
}
