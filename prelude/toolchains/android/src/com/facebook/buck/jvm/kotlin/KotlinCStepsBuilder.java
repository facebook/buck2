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

import static com.facebook.buck.jvm.kotlin.CompilerPluginUtils.getKotlinCompilerPluginsArgs;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.io.filesystem.CopySourceMode;
import com.facebook.buck.jvm.cd.command.kotlin.KotlinExtraParams;
import com.facebook.buck.jvm.cd.command.kotlin.LanguageVersion;
import com.facebook.buck.jvm.core.BuildTargetValue;
import com.facebook.buck.jvm.java.ActionMetadata;
import com.facebook.buck.jvm.java.CompilerOutputPaths;
import com.facebook.buck.jvm.java.CompilerParameters;
import com.facebook.buck.jvm.kotlin.cd.analytics.KotlinCDAnalytics;
import com.facebook.buck.jvm.kotlin.kotlinc.Kotlinc;
import com.facebook.buck.step.isolatedsteps.IsolatedStep;
import com.facebook.buck.step.isolatedsteps.common.CopyIsolatedStep;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSortedSet;
import java.nio.file.Path;
import java.util.Optional;

public class KotlinCStepsBuilder {

  static void prepareKotlinCompilation(
      RelPath buckOut,
      AbsPath buildCellRootPath,
      BuildTargetValue invokingRule,
      CompilerParameters parameters,
      ImmutableList.Builder<IsolatedStep> steps,
      ActionMetadata actionMetadata,
      KotlinExtraParams extraParams,
      RelPath kotlinClassesDir,
      String friendPathsArg,
      String kotlinPluginGeneratedFullPath,
      String moduleName,
      RelPath kotlinOutputDirectory,
      ImmutableSortedSet.Builder<RelPath> sourceWithKSPOutputBuilder,
      Path pathToSrcsList,
      ImmutableList<AbsPath> allClasspaths,
      RelPath reportsOutput,
      Kotlinc kotlinc,
      KosabiPluginOptions kosabiPluginOptions,
      KspStepsBuilder.KSPInvocationStatus kspInvocationStatus,
      ImmutableList<AbsPath> sourceOnlyAbiClasspath,
      ImmutableList.Builder<IsolatedStep> postKotlinCompilationFailureSteps,
      RelPath outputDirectory,
      ImmutableList<AbsPath> classpathSnapshots,
      KotlinCDAnalytics kotlinCDAnalytics) {
    ImmutableList.Builder<String> extraArguments =
        getKotlincExtraArguments(
            buildCellRootPath,
            invokingRule,
            parameters,
            extraParams,
            friendPathsArg,
            kotlinPluginGeneratedFullPath,
            moduleName);

    LanguageVersion kotlincLanguageVersion = extraParams.getLanguageVersion();

    if (invokingRule.isSourceOnlyAbi()) {
      kotlincLanguageVersion = LanguageVersion.Companion.getK1();
    }

    KotlincStep kotlincStep =
        new KotlincStep(
            invokingRule,
            kotlinOutputDirectory.getPath(),
            sourceWithKSPOutputBuilder.build(),
            pathToSrcsList,
            allClasspaths,
            extraParams.getKotlinHomeLibraries(),
            reportsOutput,
            kotlinc,
            extraArguments.build(),
            ImmutableList.of(CompilerPluginUtils.VERBOSE),
            parameters.getOutputPaths(),
            parameters.getShouldTrackClassUsage(),
            buckOut,
            kosabiPluginOptions.getKosabiPlugins(),
            extraParams.getKosabiJvmAbiGenEarlyTerminationMessagePrefix().orElse(null),
            invokingRule.isSourceOnlyAbi()
                && kspInvocationStatus == KspStepsBuilder.KSPInvocationStatus.KSP2_INVOKED,
            sourceOnlyAbiClasspath,
            extraParams.getShouldVerifySourceOnlyAbiConstraints(),
            postKotlinCompilationFailureSteps.build(),
            extraParams.getDepTrackerPlugin(),
            new KotlincModeFactory()
                .create(
                    invokingRule.isSourceOnlyAbi(),
                    buildCellRootPath,
                    outputDirectory.getParent().toAbsolutePath(),
                    parameters.getShouldTrackClassUsage(),
                    CompilerOutputPaths.getKotlinDepFilePath(
                        parameters.getOutputPaths().getOutputJarDirPath()),
                    extraParams,
                    Optional.ofNullable(actionMetadata),
                    classpathSnapshots),
            kotlinCDAnalytics,
            kotlincLanguageVersion);
    steps.add(kotlincStep);

    if (kotlinClassesDir != null) {
      steps.add(
          CopyIsolatedStep.forDirectory(
              kotlinOutputDirectory, outputDirectory, CopySourceMode.DIRECTORY_CONTENTS_ONLY));
    }
  }

  private static ImmutableList.Builder<String> getKotlincExtraArguments(
      AbsPath buildCellRootPath,
      BuildTargetValue invokingRule,
      CompilerParameters parameters,
      KotlinExtraParams extraParams,
      String friendPathsArg,
      String kotlinPluginGeneratedFullPath,
      String moduleName) {
    ImmutableList.Builder<String> extraArguments =
        ImmutableList.<String>builder()
            .add(friendPathsArg)
            .addAll(
                getKotlinCompilerPluginsArgs(
                    extraParams.getKotlinCompilerPlugins(),
                    kotlinPluginGeneratedFullPath,
                    KspStepsBuilder::isNotKspPlugin))
            .add(CompilerPluginUtils.MODULE_NAME)
            .add(moduleName)
            .add(CompilerPluginUtils.NO_STDLIB);

    extraParams
        .getJvmTarget()
        .ifPresent(
            target -> {
              extraArguments.add("-jvm-target");
              extraArguments.add(target);
            });

    extraArguments.addAll(extraParams.getExtraKotlincArguments());

    ImmutableList<String> jvmAbiPluginArgs =
        createJvmAbiPluginArgs(
            buildCellRootPath,
            invokingRule,
            parameters,
            extraParams,
            kotlinPluginGeneratedFullPath);
    extraArguments.addAll(jvmAbiPluginArgs);
    return extraArguments;
  }

  private static ImmutableList<String> createJvmAbiPluginArgs(
      AbsPath buildCellRootPath,
      BuildTargetValue invokingRule,
      CompilerParameters parameters,
      KotlinExtraParams extraParams,
      String kotlinPluginGeneratedFullPath) {
    // Use jvm-abi-gen for the kotlin part of class-abi, in order to get more accurate class-abi.
    // But since it is a kotlinc plugin, we produce it during library build.
    if (extraParams.getShouldUseJvmAbiGen() && invokingRule.isLibraryJar()) {
      final AbsPath jvmAbiPlugin = extraParams.getJvmAbiGenPlugin().orElseThrow();

      ImmutableMap<String, String> jvmPluginOptions =
          extraParams.getKotlinCompilerPlugins().get(jvmAbiPlugin);
      boolean hasOutputDirParam =
          Optional.ofNullable(jvmPluginOptions)
              .map(opts -> opts.containsKey(CompilerPluginUtils.JB_JVM_ABI_OUTPUT_DIR))
              .orElse(false);

      // we add 'outputDir' param, if it is missing
      if (!hasOutputDirParam) {
        final AbsPath jvmOutputDir =
            getJvmAbiGenOutputPath(buildCellRootPath, parameters, extraParams);
        ImmutableMap.Builder<String, String> jvmPluginOptionsBuilder = ImmutableMap.builder();
        if (jvmPluginOptions != null) {
          jvmPluginOptionsBuilder.putAll(jvmPluginOptions);
        }
        jvmPluginOptionsBuilder.put(
            CompilerPluginUtils.JB_JVM_ABI_OUTPUT_DIR, jvmOutputDir.toString());

        jvmPluginOptions = jvmPluginOptionsBuilder.build();
      }

      final ImmutableList<String> jvmAbiPluginArgs =
          getKotlinCompilerPluginsArgs(
              jvmAbiPlugin, jvmPluginOptions, kotlinPluginGeneratedFullPath);
      return jvmAbiPluginArgs;
    }
    return ImmutableList.of();
  }

  // when incremental compiler is on, we need to work with directory, not jar (which is used by
  // default)
  private static AbsPath getJvmAbiGenOutputPath(
      AbsPath buildCellRootPath, CompilerParameters parameters, KotlinExtraParams extraParams) {
    if (extraParams.getShouldKotlincRunIncrementally()) {
      return extraParams.getJvmAbiGenWorkingDir().get();
    }

    return buildCellRootPath.resolve(
        CompilerOutputPaths.getJvmAbiGenFilePath(
            parameters.getOutputPaths().getOutputJarDirPath()));
  }
}
