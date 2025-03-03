/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.cd.command.kotlin;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.util.immutables.BuckStyleValue;
import com.facebook.buck.jvm.java.CompileToJarStepFactory;
import com.facebook.buck.jvm.java.ResolvedJavacOptions;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSortedSet;
import java.util.Map;
import java.util.Optional;

/** Extra params for creating Kotlin compile steps. */
@BuckStyleValue
public abstract class KotlinExtraParams implements CompileToJarStepFactory.ExtraParams {

  public static final String KOTLINC_WORKING_DIR = "kotlinc_working_dir";
  public static final String KOTLINC_JVM_ABI_GEN_WORKING_DIR = "jvm_abi_gen_working_dir";

  public abstract ImmutableList<AbsPath> getExtraClassPaths();

  public abstract AbsPath getResolvedStandardLibraryClassPath();

  public abstract AbsPath getResolvedAnnotationProcessingClassPath();

  public abstract AnnotationProcessingTool getAnnotationProcessingTool();

  public abstract ImmutableList<String> getExtraKotlincArguments();

  public LanguageVersion getSanitizedLanguageVersion() {
    String languageVersion = getLanguageVersion();
    if (languageVersion == null) {
      throw new IllegalStateException("Language version is not set");
    }
    return new LanguageVersion(languageVersion);
  }

  protected abstract String getLanguageVersion();

  public abstract ImmutableMap<AbsPath, ImmutableMap<String, String>>
      getResolvedKotlinCompilerPlugins();

  public abstract ImmutableMap<String, AbsPath> getResolvedKosabiPluginOptionPath();

  public abstract Optional<String> getKosabiJvmAbiGenEarlyTerminationMessagePrefix();

  public abstract ImmutableSortedSet<AbsPath> getResolvedFriendPaths();

  public abstract ImmutableList<AbsPath> getResolvedKotlinHomeLibraries();

  public abstract ResolvedJavacOptions getResolvedJavacOptions();

  public abstract Optional<String> getJvmTarget();

  public abstract boolean shouldGenerateAnnotationProcessingStats();

  public abstract boolean shouldUseJvmAbiGen();

  public abstract Optional<AbsPath> getJvmAbiGenPlugin();

  public abstract boolean shouldVerifySourceOnlyAbiConstraints();

  public abstract boolean shouldRemoveKotlinCompilerFromClassPath();

  public abstract boolean shouldKotlincRunViaBuildToolsApi();

  public abstract boolean shouldKotlincRunIncrementally();

  public abstract Optional<AbsPath> getIncrementalStateDir();

  public abstract Optional<AbsPath> getDepTrackerPlugin();

  public abstract boolean shouldUseStandaloneKosabi();

  public Optional<AbsPath> getKotlincWorkingDir() {
    return getIncrementalStateDir().map(dir -> dir.resolve(KOTLINC_WORKING_DIR));
  }

  public Optional<AbsPath> getJvmAbiGenWorkingDir() {
    return getIncrementalStateDir().map(dir -> dir.resolve(KOTLINC_JVM_ABI_GEN_WORKING_DIR));
  }

  /** Package extra params that were resolved before. */
  public static KotlinExtraParams of(
      ImmutableList<AbsPath> extraClassPaths,
      AbsPath standardLibraryClassPath,
      AbsPath annotationProcessingClassPath,
      AnnotationProcessingTool annotationProcessingTool,
      ImmutableList<String> extraKotlincArguments,
      Map<AbsPath, ImmutableMap<String, String>> kotlinCompilerPlugins,
      Map<String, AbsPath> kosabiPluginOptions,
      Optional<String> kosabiJvmAbiGenEarlyTerminationMessagePrefix,
      ImmutableSortedSet<AbsPath> friendPaths,
      ImmutableSortedSet<AbsPath> kotlinHomeLibraries,
      ResolvedJavacOptions resolvedJavacOptions,
      Optional<String> jvmTarget,
      boolean shouldGenerateAnnotationProcessingStats,
      boolean shouldUseJvmAbiGen,
      Optional<AbsPath> jvmAbiGenPlugin,
      boolean shouldVerifySourceOnlyAbiConstraints,
      boolean shouldRemoveKotlinCompilerFromClassPath,
      Optional<AbsPath> depTrackerPlugin,
      boolean shouldKotlincRunViaBuildToolsApi,
      boolean shouldKotlincRunIncrementally,
      boolean shouldUseStandaloneKosabi,
      Optional<AbsPath> incrementalStateDir,
      String languageVersion) {
    return ImmutableKotlinExtraParams.ofImpl(
        extraClassPaths,
        standardLibraryClassPath,
        annotationProcessingClassPath,
        annotationProcessingTool,
        extraKotlincArguments,
        languageVersion,
        kotlinCompilerPlugins,
        kosabiPluginOptions,
        kosabiJvmAbiGenEarlyTerminationMessagePrefix,
        friendPaths,
        kotlinHomeLibraries,
        resolvedJavacOptions,
        jvmTarget,
        shouldGenerateAnnotationProcessingStats,
        shouldUseJvmAbiGen,
        jvmAbiGenPlugin,
        shouldVerifySourceOnlyAbiConstraints,
        shouldRemoveKotlinCompilerFromClassPath,
        shouldKotlincRunViaBuildToolsApi,
        shouldKotlincRunIncrementally,
        incrementalStateDir,
        depTrackerPlugin,
        shouldUseStandaloneKosabi);
  }
}
