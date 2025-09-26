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
import com.facebook.buck.jvm.cd.command.kotlin.LanguageVersion;
import com.facebook.buck.jvm.core.BuildTargetValue;
import com.facebook.buck.jvm.java.CompilerOutputPaths;
import com.facebook.buck.jvm.kotlin.cd.analytics.KotlinCDAnalytics;
import com.facebook.buck.jvm.kotlin.kotlinc.Kotlinc;
import com.facebook.buck.jvm.kotlin.kotlinc.incremental.KotlincMode;
import com.facebook.buck.step.isolatedsteps.IsolatedStep;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSortedSet;
import java.nio.file.Path;
import java.util.Optional;
import javax.annotation.Nullable;

/**
 * Kosabi Stubgen Step A step to run Kosabi's stubgen (generate stubs & run source modifier to
 * stripped srcs) for source only build Currently, it's based on KotlinC and simply run existing
 * stubgen plugins with configurations to output stripped srcs as artifact for further compilation
 * steps Later we could move away from KotlinC and explore possibility to use Kotlin Analysis API
 * for output stripped srcs
 */
public class KosabiStubgenStep extends KotlincStep {
  private final RelPath stubgenDir;
  private final RelPath stubClassOutputDir;

  private final String pluginPath;

  KosabiStubgenStep(
      BuildTargetValue invokingRule,
      Path outputDirectory,
      ImmutableSortedSet<RelPath> sourceFilePaths,
      Path pathToSrcsList,
      ImmutableList<AbsPath> combinedClassPathEntries,
      ImmutableList<AbsPath> kotlinHomeLibraries,
      RelPath reportDirPath,
      Kotlinc kotlinc,
      ImmutableList<String> extraArguments,
      ImmutableList<String> verboseModeOnlyExtraArguments,
      CompilerOutputPaths outputPaths,
      boolean trackClassUsage,
      RelPath configuredBuckOut,
      ImmutableMap<String, AbsPath> resolvedKosabiPluginOptionPath,
      @Nullable String kosabiJvmAbiGenEarlyTerminationMessagePrefix,
      boolean kosabiShouldEnableMixedCompilation,
      ImmutableList<AbsPath> sourceOnlyAbiClasspath,
      boolean verifySourceOnlyAbiConstraints,
      ImmutableList<IsolatedStep> postKotlinCompilationFailureSteps,
      Optional<AbsPath> depTrackerPath,
      RelPath stubgenDir,
      @Nullable RelPath stubClassOutputDir,
      KotlinCDAnalytics kotlinCDAnalytics,
      LanguageVersion languageVersion) {

    super(
        invokingRule,
        outputDirectory,
        sourceFilePaths,
        pathToSrcsList,
        combinedClassPathEntries,
        kotlinHomeLibraries,
        reportDirPath,
        kotlinc,
        extraArguments,
        verboseModeOnlyExtraArguments,
        outputPaths,
        false, // trackClassUsage
        configuredBuckOut,
        resolvedKosabiPluginOptionPath,
        kosabiJvmAbiGenEarlyTerminationMessagePrefix,
        kosabiShouldEnableMixedCompilation,
        sourceOnlyAbiClasspath,
        verifySourceOnlyAbiConstraints,
        postKotlinCompilationFailureSteps,
        depTrackerPath,
        KotlincMode.NonIncremental.INSTANCE,
        kotlinCDAnalytics,
        languageVersion,
        // Flag turning on/off K2 support for jvm-abi-gen actions
        // not part of stubsgen, since jvm-abi-gen doesn't run on stubsgen steps
        false);
    this.stubgenDir = stubgenDir;
    this.stubClassOutputDir = stubClassOutputDir;
    this.pluginPath =
        "plugin:"
            + (languageVersion.getSupportsK2()
                ? "com.facebook.kotlin.compilerplugins.kosabi.stubsgen_k2"
                : "com.facebook.kotlin.compilerplugins.kosabi.stubsgen");
  }

  @Override
  public String getShortName() {
    return KosabiStubgenStep.class.getSimpleName();
  }

  @Override
  protected void configureSourceOnlyOptions(
      ImmutableList.Builder<String> builder, LanguageVersion languageVersion) {
    super.configureSourceOnlyOptions(builder, languageVersion);
    builder.add("-P");
    builder.add(pluginPath + ":stubsgen-dir=" + stubgenDir.toString());

    if (stubClassOutputDir != null) {
      builder.add("-P");
      builder.add(pluginPath + ":stubs-class-dir=" + stubClassOutputDir.toString());
    }
  }
}
