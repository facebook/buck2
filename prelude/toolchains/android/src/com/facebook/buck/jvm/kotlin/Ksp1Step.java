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
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSortedSet;
import java.nio.file.Path;
import java.util.Optional;
import javax.annotation.Nullable;

/**
 * Triggering only the KSP1 plugin. Currently, KSP1 plugin needs its own kotlinc invocation, which
 * will change with KSP2. In case of source-only builds, we should have sufficient source-only ABI
 * generated after running this step.
 */
public class Ksp1Step extends KotlincStep {

  private static final String VERBOSE = "-verbose";

  Ksp1Step(
      BuildTargetValue invokingRule,
      Path outputDirectory,
      ImmutableSortedSet<RelPath> sourceFilePaths,
      Path pathToSrcsList,
      ImmutableList<AbsPath> combinedClassPathEntries,
      ImmutableList<AbsPath> kotlinHomeLibraries,
      RelPath reportDirPath,
      Kotlinc kotlinc,
      ImmutableList<String> extraArguments,
      CompilerOutputPaths outputPaths,
      RelPath configuredBuckOut,
      ImmutableMap<String, AbsPath> resolvedKosabiPluginOptionPath,
      @Nullable String kosabiEarlyTerminationMessagePrefix,
      ImmutableList<AbsPath> sourceOnlyAbiClasspath,
      KotlinCDAnalytics kotlinCDAnalytics) {
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
        ImmutableList.of(VERBOSE),
        outputPaths,
        false,
        configuredBuckOut,
        resolvedKosabiPluginOptionPath,
        kosabiEarlyTerminationMessagePrefix,
        true,
        sourceOnlyAbiClasspath,
        false,
        ImmutableList.of(),
        Optional.empty(),
        KotlincMode.NonIncremental.INSTANCE,
        kotlinCDAnalytics,
        // force K1 for KSP1
        LanguageVersion.Companion.getK1());
  }

  @Override
  public String getShortName() {
    return "ksp1";
  }
}
