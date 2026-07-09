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

import static com.facebook.buck.jvm.java.JavaPaths.SRC_ZIP;
import static com.facebook.buck.jvm.kotlin.AnnotationProcessorUtils.getAnnotationProcessors;
import static com.facebook.buck.jvm.kotlin.CompilerPluginUtils.getKotlinCompilerPluginsArgs;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.io.filesystem.CopySourceMode;
import com.facebook.buck.jvm.cd.command.kotlin.AnnotationProcessingTool;
import com.facebook.buck.jvm.cd.command.kotlin.KotlinExtraParams;
import com.facebook.buck.jvm.core.BuildTargetValue;
import com.facebook.buck.jvm.core.BuildTargetValueExtraParams;
import com.facebook.buck.jvm.java.ActionMetadata;
import com.facebook.buck.jvm.java.CompilerOutputPaths;
import com.facebook.buck.jvm.java.JavacPluginParams;
import com.facebook.buck.jvm.java.ResolvedJavacPluginProperties;
import com.facebook.buck.jvm.kotlin.cd.analytics.KotlinCDAnalytics;
import com.facebook.buck.jvm.kotlin.kotlinc.Kotlinc;
import com.facebook.buck.jvm.kotlin.ksp.Ksp2Step;
import com.facebook.buck.step.isolatedsteps.IsolatedStep;
import com.facebook.buck.step.isolatedsteps.common.CopyIsolatedStep;
import com.facebook.buck.step.isolatedsteps.common.MakeCleanDirectoryIsolatedStep;
import com.facebook.buck.step.isolatedsteps.common.MkdirIsolatedStep;
import com.facebook.buck.step.isolatedsteps.common.ZipIsolatedStep;
import com.facebook.buck.util.zip.ZipCompressionLevel;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.ImmutableSortedSet;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;

public class KspStepsBuilder {
  private static final String KSP_PLUGIN_ID = "plugin:com.google.devtools.ksp.symbol-processing:";
  private static final String MODULE_NAME = "-module-name";
  private static final String PLUGIN = "-P";

  /** Initialize all the folders, steps and parameters needed to run KSP plugins for this rule. */
  public static KSPInvocationStatus prepareKspProcessorsIfNeeded(
      Optional<ActionMetadata> actionMetadata,
      KotlinExtraParams extraParams,
      BuildTargetValue invokingRule,
      AbsPath rootPath,
      ImmutableList.Builder<IsolatedStep> steps,
      ImmutableList.Builder<IsolatedStep> postKotlinCompilationSteps,
      BuildTargetValueExtraParams buildTargetValueExtraParams,
      RelPath outputDirectory,
      RelPath annotationGenFolder,
      ImmutableSortedSet.Builder<RelPath> javacSourceBuilder,
      RelPath reportsOutput,
      boolean shouldTrackClassUsage,
      ImmutableList<AbsPath> allClasspaths,
      String kotlinPluginGeneratedOutFullPath,
      RelPath projectBaseDir,
      JavacPluginParams annotationProcessorParams,
      ImmutableSortedSet<RelPath> sourceFilePaths,
      Path pathToSrcsList,
      ImmutableList<AbsPath> kotlinHomeLibraries,
      Kotlinc kotlinc,
      CompilerOutputPaths compilerOutputPaths,
      RelPath configuredBuckOut,
      ImmutableMap<String, AbsPath> resolvedKosabiPluginOptionPath,
      ImmutableSortedSet.Builder<RelPath> sourceBuilderWithKspOutputs,
      ImmutableList<AbsPath> compilationClasspath,
      String moduleName,
      KotlinCDAnalytics kotlinCDAnalytics) {

    ImmutableList<ResolvedJavacPluginProperties> kspAnnotationProcessors =
        getKspAnnotationProcessors(getAnnotationProcessors(annotationProcessorParams));

    KSPInvocationStatus kspInvocationStatus = KSPInvocationStatus.NOT_INVOKED;

    // The other option is to use JAVAC, and we don't want to use KSP in that case.
    if (!extraParams.getAnnotationProcessingTool().equals(AnnotationProcessingTool.KAPT)) {
      return kspInvocationStatus;
    }

    // We need to generate the KSP generation folder anyway, to help IntelliJ with red
    // symbols.
    RelPath kspAnnotationGenFolder = buildTargetValueExtraParams.getKspAnnotationGenPath();
    steps.addAll(MakeCleanDirectoryIsolatedStep.of(kspAnnotationGenFolder));

    if (kspAnnotationProcessors.isEmpty()) {
      return kspInvocationStatus;
    }

    steps.add(new MkdirIsolatedStep(outputDirectory));
    // KSP folders
    RelPath kspOutputBaseDir = buildTargetValueExtraParams.getAnnotationOutputBasePath();
    RelPath kspClassesOutput =
        buildTargetValueExtraParams.getAnnotationOutputPath("__%s_ksp_classes__");
    RelPath kspKotlinOutput =
        buildTargetValueExtraParams.getAnnotationOutputPath("__%s_ksp_generated_kotlin__");
    RelPath kspJavaOutput =
        buildTargetValueExtraParams.getAnnotationOutputPath("__%s_ksp_generated_java__");
    RelPath kspResOutput =
        buildTargetValueExtraParams.getAnnotationOutputPath("__%s_ksp_res_output__");
    RelPath kspCachesOutput =
        buildTargetValueExtraParams.getAnnotationOutputPath("__%s_ksp_cache_output__");
    RelPath kspMetaOutput =
        buildTargetValueExtraParams.getAnnotationOutputPath("__%s_ksp_meta_output__");

    // More KSP folders
    RelPath kspGenOutputFolder = buildTargetValueExtraParams.getGenPath("__%s_ksp_gen_sources__");
    RelPath kspGenOutput =
        buildTargetValueExtraParams.getGenPath("__%s_ksp_gen_sources__/generated" + SRC_ZIP);

    // Creating KSP dirs
    steps.addAll(MakeCleanDirectoryIsolatedStep.of(kspClassesOutput));
    steps.addAll(MakeCleanDirectoryIsolatedStep.of(kspKotlinOutput));
    steps.addAll(MakeCleanDirectoryIsolatedStep.of(kspJavaOutput));
    steps.addAll(MakeCleanDirectoryIsolatedStep.of(kspResOutput));
    steps.addAll(MakeCleanDirectoryIsolatedStep.of(kspCachesOutput));
    steps.addAll(MakeCleanDirectoryIsolatedStep.of(kspMetaOutput));
    steps.addAll(MakeCleanDirectoryIsolatedStep.of(kspGenOutputFolder));

    ImmutableList<String> kspProcessorsClasspathList =
        kspAnnotationProcessors.stream()
            .map(p -> p.toUrlClasspath(rootPath))
            .flatMap(List::stream)
            .map(AnnotationProcessorUtils::urlToFile)
            .collect(ImmutableList.toImmutableList());

    kspInvocationStatus = KSPInvocationStatus.KSP2_INVOKED;

    ImmutableList.Builder<AbsPath> allClassPathsBuilder = ImmutableList.builder();

    allClassPathsBuilder.addAll(allClasspaths);

    if (invokingRule.isSourceOnlyAbi()) {
      allClassPathsBuilder.addAll(
          compilationClasspath.stream().filter(p -> !allClasspaths.contains(p)).toList());
    }

    Ksp2Step ksp2Step =
        new Ksp2Step(
            invokingRule,
            compilerOutputPaths,
            rootPath,
            shouldTrackClassUsage,
            allClassPathsBuilder.build(),
            kotlinPluginGeneratedOutFullPath,
            annotationProcessorParams.getParameters(),
            sourceFilePaths,
            CompilerOutputPaths.getKspDepFilePath(reportsOutput),
            moduleName,
            kspProcessorsClasspathList,
            kspClassesOutput,
            kspKotlinOutput,
            kspJavaOutput,
            kspOutputBaseDir,
            extraParams.getJvmTarget(),
            extraParams.getLanguageVersion(),
            getJvmDefaultMode(extraParams.getExtraKotlincArguments()),
            extraParams.getJavaBinary(),
            kotlinCDAnalytics,
            Ksp2ModeFactory.create(
                rootPath,
                invokingRule.isSourceOnlyAbi(),
                kspCachesOutput,
                extraParams,
                actionMetadata.orElse(null)));
    steps.add(ksp2Step);
    steps.add(
        CopyIsolatedStep.forDirectory(
            kspKotlinOutput, kspAnnotationGenFolder, CopySourceMode.DIRECTORY_CONTENTS_ONLY));
    steps.add(
        CopyIsolatedStep.forDirectory(
            kspJavaOutput, kspAnnotationGenFolder, CopySourceMode.DIRECTORY_CONTENTS_ONLY));
    steps.add(
        CopyIsolatedStep.forDirectory(
            kspClassesOutput, kspAnnotationGenFolder, CopySourceMode.DIRECTORY_CONTENTS_ONLY));

    steps.add(
        new ZipIsolatedStep(
            rootPath,
            kspGenOutput.getPath(),
            ImmutableSet.of(),
            ImmutableSet.of(),
            false,
            ZipCompressionLevel.DEFAULT,
            kspAnnotationGenFolder.getPath()));

    steps.add(
        CopyIsolatedStep.forDirectory(
            kspAnnotationGenFolder, annotationGenFolder, CopySourceMode.DIRECTORY_CONTENTS_ONLY));

    // Generated classes should be part of the output. This way generated files such as
    // META-INF dirs will also be added to the final jar.
    postKotlinCompilationSteps.add(
        CopyIsolatedStep.forDirectory(
            kspClassesOutput.getPath(),
            outputDirectory.getPath(),
            CopySourceMode.DIRECTORY_CONTENTS_ONLY));

    sourceBuilderWithKspOutputs.add(kspGenOutput);

    javacSourceBuilder.add(kspGenOutput);

    return kspInvocationStatus;
  }

  private static ImmutableList<String> getKspPluginsArgs(
      ImmutableMap<AbsPath, ImmutableMap<String, String>> resolvedKotlinCompilerPlugins,
      String outputDir) {
    return getKotlinCompilerPluginsArgs(
        resolvedKotlinCompilerPlugins,
        outputDir,
        KspStepsBuilder::isPluginRequiredForStandaloneKsp);
  }

  private static String getJvmDefaultMode(ImmutableList<String> args) {
    for (String arg : args) {
      String[] splitArg = arg.split("=");
      if (splitArg.length == 2) {
        if (splitArg[0].equals("-Xjvm-default")) {
          return splitArg[1];
        }
        // Kotlin 2.3 renamed -Xjvm-default to -jvm-default with new mode names. Map them back to
        // the legacy mode names KSP expects to preserve behavior.
        if (splitArg[0].equals("-jvm-default")) {
          switch (splitArg[1]) {
            case "no-compatibility":
              return "all";
            case "enable":
              return "all-compatibility";
            case "disable":
              return "disable";
            default:
              return splitArg[1];
          }
        }
      }
    }
    return "disabled";
  }

  private static boolean isPluginRequiredForStandaloneKsp(
      AbsPath sourcePath, ImmutableMap<String, String> options) {
    return isKspPlugin(sourcePath) || CompilerPluginUtils.isDiK1PluginForKsp(sourcePath, options);
  }

  private static boolean isPluginNotRequiredForStandaloneKsp(
      AbsPath sourcePath, ImmutableMap<String, String> options) {
    return !isPluginRequiredForStandaloneKsp(sourcePath, options);
  }

  private static boolean isKspPlugin(AbsPath sourcePath) {
    return sourcePath.toString().contains("symbol-processing");
  }

  public static boolean isNotKspPlugin(AbsPath sourcePath, ImmutableMap<String, String> options) {
    return !isKspPlugin(sourcePath);
  }

  static ImmutableList<ResolvedJavacPluginProperties> getKspAnnotationProcessors(
      ImmutableList<ResolvedJavacPluginProperties> annotationProcessors) {
    return annotationProcessors.stream()
        .filter(AnnotationProcessorUtils::isKSPProcessor)
        .collect(ImmutableList.toImmutableList());
  }

  public enum KSPInvocationStatus {
    KSP2_INVOKED,
    NOT_INVOKED
  }
}
