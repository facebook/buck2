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

import static com.facebook.buck.jvm.java.CompilerOutputPaths.getKspDepFilePath;
import static com.facebook.buck.jvm.java.JavaPaths.SRC_ZIP;
import static com.facebook.buck.jvm.kotlin.AnnotationProcessorUtils.getAnnotationProcessors;
import static com.facebook.buck.jvm.kotlin.CompilerPluginUtils.getKotlinCompilerPluginsArgs;
import static com.google.common.collect.Iterables.transform;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.io.filesystem.CopySourceMode;
import com.facebook.buck.jvm.cd.command.kotlin.AnnotationProcessingTool;
import com.facebook.buck.jvm.cd.command.kotlin.LanguageVersion;
import com.facebook.buck.jvm.core.BuildTargetValue;
import com.facebook.buck.jvm.core.BuildTargetValueExtraParams;
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
import com.google.common.base.Joiner;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.ImmutableSortedSet;
import java.io.File;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;

public class KspStepsBuilder {
  private static final String KSP_PLUGIN_ID = "plugin:com.google.devtools.ksp.symbol-processing:";
  private static final String MODULE_NAME = "-module-name";
  private static final String PLUGIN = "-P";

  /** Initialize all the folders, steps and parameters needed to run KSP plugins for this rule. */
  public static KSPInvocationStatus prepareKspProcessorsIfNeeded(
      AnnotationProcessingTool annotationProcessingTool,
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
      ImmutableMap<AbsPath, ImmutableMap<String, String>> resolvedKotlinCompilerPlugins,
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
      String kosabiJvmAbiGenEarlyTerminationMessagePrefix,
      ImmutableSortedSet.Builder<RelPath> sourceBuilderWithKspOutputs,
      ImmutableList<AbsPath> sourceOnlyAbiClasspath,
      String moduleName,
      Optional<String> jvmTarget,
      ImmutableList<String> extraKotlincArguments,
      KotlinCDAnalytics kotlinCDAnalytics,
      LanguageVersion languageVersion) {

    ImmutableList<ResolvedJavacPluginProperties> kspAnnotationProcessors =
        getKspAnnotationProcessors(getAnnotationProcessors(annotationProcessorParams));

    KSPInvocationStatus kspInvocationStatus = KSPInvocationStatus.NOT_INVOKED;

    // The other option is to use JAVAC, and we don't want to use KSP in that case.
    if (!annotationProcessingTool.equals(AnnotationProcessingTool.KAPT)) {
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
    RelPath kspClassesOutput = buildTargetValueExtraParams.getAnnotationPath("__%s_ksp_classes__");
    RelPath kspKotlinOutput =
        buildTargetValueExtraParams.getAnnotationPath("__%s_ksp_generated_kotlin__");
    RelPath kspJavaOutput =
        buildTargetValueExtraParams.getAnnotationPath("__%s_ksp_generated_java__");
    RelPath kspGenOutputFolder = buildTargetValueExtraParams.getGenPath("__%s_ksp_gen_sources__");
    RelPath kspGenOutput =
        buildTargetValueExtraParams.getGenPath("__%s_ksp_gen_sources__/generated" + SRC_ZIP);

    // More KSP folders
    RelPath kspResOutput = buildTargetValueExtraParams.getAnnotationPath("__%s_ksp_res_output__");
    RelPath kspCachesOutput =
        buildTargetValueExtraParams.getAnnotationPath("__%s_ksp_cache_output__");
    RelPath kspOutput = buildTargetValueExtraParams.getAnnotationPath("__%s_ksp_meta_output__");

    // Creating KSP dirs
    steps.addAll(MakeCleanDirectoryIsolatedStep.of(kspClassesOutput));
    steps.addAll(MakeCleanDirectoryIsolatedStep.of(kspKotlinOutput));
    steps.addAll(MakeCleanDirectoryIsolatedStep.of(kspJavaOutput));
    steps.addAll(MakeCleanDirectoryIsolatedStep.of(kspGenOutputFolder));

    steps.addAll(MakeCleanDirectoryIsolatedStep.of(kspResOutput));
    steps.addAll(MakeCleanDirectoryIsolatedStep.of(kspCachesOutput));
    steps.addAll(MakeCleanDirectoryIsolatedStep.of(kspOutput));

    ImmutableList<String> kspProcessorsClasspathList =
        kspAnnotationProcessors.stream()
            .map(p -> p.toUrlClasspath(rootPath))
            .flatMap(List::stream)
            .map(AnnotationProcessorUtils::urlToFile)
            .collect(ImmutableList.toImmutableList());

    if (isKsp2(annotationProcessorParams)) {
      kspInvocationStatus = KSPInvocationStatus.KSP2_INVOKED;

      ImmutableList.Builder<AbsPath> allClassPathsBuilder = ImmutableList.builder();

      allClassPathsBuilder.addAll(allClasspaths);

      if (invokingRule.isSourceOnlyAbi()) {
        allClassPathsBuilder.addAll(
            sourceOnlyAbiClasspath.stream().filter(p -> !allClasspaths.contains(p)).toList());
      }

      Ksp2Step ksp2Step =
          new Ksp2Step(
              invokingRule,
              compilerOutputPaths,
              rootPath,
              shouldTrackClassUsage,
              allClassPathsBuilder.build(),
              kotlinPluginGeneratedOutFullPath,
              projectBaseDir,
              annotationProcessorParams.getParameters(),
              sourceFilePaths,
              CompilerOutputPaths.getKspDepFilePath(reportsOutput),
              moduleName,
              kspProcessorsClasspathList,
              kspClassesOutput,
              kspKotlinOutput,
              kspJavaOutput,
              kspCachesOutput,
              kspOutput,
              jvmTarget,
              languageVersion,
              getJvmDefaultMode(extraKotlincArguments),
              kotlinCDAnalytics);
      steps.add(ksp2Step);
    } else {
      kspInvocationStatus = KSPInvocationStatus.KSP1_INVOKED;
      String kspProcessorsClasspath =
          Joiner.on(File.pathSeparatorChar).join(kspProcessorsClasspathList);
      steps.add(
          getKsp1Step(
              invokingRule,
              rootPath,
              outputDirectory,
              reportsOutput,
              shouldTrackClassUsage,
              allClasspaths,
              resolvedKotlinCompilerPlugins,
              kotlinPluginGeneratedOutFullPath,
              projectBaseDir,
              annotationProcessorParams,
              sourceFilePaths,
              pathToSrcsList,
              kotlinHomeLibraries,
              kotlinc,
              compilerOutputPaths,
              configuredBuckOut,
              resolvedKosabiPluginOptionPath,
              kosabiJvmAbiGenEarlyTerminationMessagePrefix,
              sourceOnlyAbiClasspath,
              moduleName,
              extraKotlincArguments,
              kspProcessorsClasspath,
              kspClassesOutput,
              kspKotlinOutput,
              kspJavaOutput,
              kspCachesOutput,
              kspOutput,
              kotlinCDAnalytics));
    }

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

  private static Ksp1Step getKsp1Step(
      BuildTargetValue invokingRule,
      AbsPath rootPath,
      RelPath outputDirectory,
      RelPath reportsOutput,
      boolean shouldTrackClassUsage,
      ImmutableList<AbsPath> allClasspaths,
      ImmutableMap<AbsPath, ImmutableMap<String, String>> resolvedKotlinCompilerPlugins,
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
      String kosabiJvmAbiGenEarlyTerminationMessagePrefix,
      ImmutableList<AbsPath> sourceOnlyAbiClasspath,
      String moduleName,
      ImmutableList<String> extraKotlincArguments,
      String kspProcessorsClasspath,
      RelPath kspClassesOutput,
      RelPath kspKotlinOutput,
      RelPath kspJavaOutput,
      RelPath kspCachesOutput,
      RelPath kspOutput,
      KotlinCDAnalytics kotlinCDAnalytics) {
    ImmutableList.Builder<String> kspPluginOptionsBuilder = ImmutableList.builder();
    kspPluginOptionsBuilder
        .add(KSP_PLUGIN_ID + "apclasspath=" + kspProcessorsClasspath)
        .add(KSP_PLUGIN_ID + "projectBaseDir=" + rootPath.resolve(projectBaseDir))
        .add(KSP_PLUGIN_ID + "classOutputDir=" + rootPath.resolve(kspClassesOutput))
        .add(KSP_PLUGIN_ID + "kotlinOutputDir=" + rootPath.resolve(kspKotlinOutput))
        .add(KSP_PLUGIN_ID + "javaOutputDir=" + rootPath.resolve(kspJavaOutput))
        .add(KSP_PLUGIN_ID + "resourceOutputDir=" + rootPath.resolve(kspClassesOutput))
        .add(KSP_PLUGIN_ID + "cachesDir=" + rootPath.resolve(kspCachesOutput))
        .add(KSP_PLUGIN_ID + "kspOutputDir=" + rootPath.resolve(kspOutput));

    // KSP needs the full classpath in order to resolve resources
    String allClasspath =
        Joiner.on(File.pathSeparator)
            .join(transform(allClasspaths, path -> path.getPath().toString()));
    allClasspath = allClasspath.replace(',', '-');
    kspPluginOptionsBuilder.add(KSP_PLUGIN_ID + "apoption=" + "cp=" + allClasspath);

    if (shouldTrackClassUsage) {
      kspPluginOptionsBuilder.add(
          KSP_PLUGIN_ID
              + "apoption="
              + "fileAccessHistoryReportFile="
              + rootPath.resolve(getKspDepFilePath(reportsOutput)));
    }

    if (invokingRule.isSourceOnlyAbi()) {
      kspPluginOptionsBuilder
          .add(KSP_PLUGIN_ID + "apoption=" + "com.facebook.buck.kotlin.generating_abi=" + "true")
          .add(KSP_PLUGIN_ID + "withCompilation=true");
    }

    ImmutableSortedSet<String> apParams = annotationProcessorParams.getParameters();
    for (String param : apParams) {
      Preconditions.checkState(param.split("=").length == 2);
      kspPluginOptionsBuilder.add(KSP_PLUGIN_ID + "apoption=" + param);
    }

    kspPluginOptionsBuilder.add(
        KSP_PLUGIN_ID
            + "apoption=com.facebook.buck.kotlin.ksp_generated_out_path="
            + kotlinPluginGeneratedOutFullPath);

    boolean forceK1ForKspPlugins = true;

    ImmutableList.Builder<String> kspTriggerBuilder = ImmutableList.builder();
    kspTriggerBuilder
        .addAll(getKspPluginsArgs(resolvedKotlinCompilerPlugins, kotlinPluginGeneratedOutFullPath))
        .add(PLUGIN, Joiner.on(",").join(kspPluginOptionsBuilder.build()));

    kspTriggerBuilder.add(MODULE_NAME).add(moduleName);

    // To support source-only compilation, which only runs once without executing the main
    // compilation, we need to provide additional arguments to the Kotlin compiler (kotlinc) to
    // align the JVM settings and compiler plugin arguments for customizations in the non-ksp plugin
    // settings.
    if (invokingRule.isSourceOnlyAbi()) {
      kspTriggerBuilder
          .addAll(
              getKotlinCompilerPluginsArgs(
                  resolvedKotlinCompilerPlugins,
                  kotlinPluginGeneratedOutFullPath,
                  KspStepsBuilder::isPluginNotRequiredForStandaloneKsp))
          .addAll(extraKotlincArguments);
    }

    return new Ksp1Step(
        invokingRule,
        outputDirectory.getPath(),
        sourceFilePaths,
        pathToSrcsList,
        allClasspaths,
        kotlinHomeLibraries,
        reportsOutput,
        kotlinc,
        kspTriggerBuilder.build(),
        compilerOutputPaths,
        configuredBuckOut,
        resolvedKosabiPluginOptionPath,
        kosabiJvmAbiGenEarlyTerminationMessagePrefix,
        sourceOnlyAbiClasspath,
        kotlinCDAnalytics);
  }

  private static boolean isKsp2(JavacPluginParams annotationProcessorParams) {
    ImmutableSortedSet<String> apParams = annotationProcessorParams.getParameters();
    for (String param : apParams) {
      String[] splitParam = param.split("=");
      Preconditions.checkState(splitParam.length == 2);
      if (splitParam[0].equals("com.facebook.ksp.isKSP2")) {
        return Boolean.parseBoolean(splitParam[1]);
      }
    }
    return false;
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
      if (splitArg.length == 2 && splitArg[0].equals("-Xjvm-default")) {
        return splitArg[1];
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
    KSP1_INVOKED,
    KSP2_INVOKED,
    NOT_INVOKED
  }
}
