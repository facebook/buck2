/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.kotlin;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.jvm.cd.AbiStepsBuilder;
import com.facebook.buck.jvm.cd.BuildCommandStepsBuilder;
import com.facebook.buck.jvm.cd.DefaultCompileStepsBuilderFactory;
import com.facebook.buck.jvm.cd.LibraryStepsBuilder;
import com.facebook.buck.jvm.cd.command.BaseJarCommand;
import com.facebook.buck.jvm.cd.command.BuildMode;
import com.facebook.buck.jvm.cd.command.kotlin.BuildKotlinCommand;
import com.facebook.buck.jvm.cd.command.kotlin.KotlinExtraParams;
import com.facebook.buck.jvm.java.ActionMetadata;
import com.facebook.buck.jvm.kotlin.cd.analytics.KotlinCDAnalytics;
import com.facebook.buck.step.isolatedsteps.IsolatedStep;
import com.google.common.collect.ImmutableList;
import javax.annotation.Nullable;

/** Converts serialized protobuf commands into steps to compile Kotlin targets. */
public class KotlinStepsBuilder implements BuildCommandStepsBuilder {
  private final ImmutableList<IsolatedStep> steps;
  private final AbsPath ruleCellRoot;

  public KotlinStepsBuilder(
      BuildKotlinCommand buildKotlinCommand,
      @Nullable ActionMetadata actionMetadata,
      @Nullable RelPath kotlinClassesDir,
      KotlinCDAnalytics kotlinCDAnalytics) {
    steps = buildSteps(buildKotlinCommand, actionMetadata, kotlinClassesDir, kotlinCDAnalytics);
    ruleCellRoot = buildKotlinCommand.getBaseJarCommand().getBuildCellRootPath();
  }

  /**
   * @return the steps corresponding to the protobuf command
   */
  @Override
  public ImmutableList<IsolatedStep> getSteps() {
    return steps;
  }

  /**
   * @return the rule cell root.
   */
  @Override
  public AbsPath getRuleCellRoot() {
    return ruleCellRoot;
  }

  private ImmutableList<IsolatedStep> buildSteps(
      BuildKotlinCommand buildKotlinCommand,
      @Nullable ActionMetadata actionMetadata,
      RelPath kotlinClassesDir,
      KotlinCDAnalytics kotlinCDAnalytics) {
    DaemonKotlincToJarStepFactory kotlincToJarStepFactory =
        new DaemonKotlincToJarStepFactory(kotlinCDAnalytics);

    DefaultCompileStepsBuilderFactory<KotlinExtraParams> stepsBuilderFactory =
        new DefaultCompileStepsBuilderFactory<>(kotlincToJarStepFactory);

    if (buildKotlinCommand.getBuildMode() == BuildMode.LIBRARY) {
      return handleLibaryJarCommand(
          stepsBuilderFactory,
          buildKotlinCommand.getBaseJarCommand(),
          buildKotlinCommand.getKotlinExtraParams(),
          actionMetadata,
          kotlinClassesDir);
    } else {
      return handleAbiJarCommand(
          stepsBuilderFactory,
          buildKotlinCommand.getBaseJarCommand(),
          buildKotlinCommand.getKotlinExtraParams());
    }
  }

  private ImmutableList<IsolatedStep> handleLibaryJarCommand(
      DefaultCompileStepsBuilderFactory<KotlinExtraParams> stepsBuilderFactory,
      BaseJarCommand baseJarCommand,
      KotlinExtraParams kotlinExtraParams,
      @Nullable ActionMetadata actionMetadata,
      @Nullable RelPath kotlinClassesDir) {

    LibraryStepsBuilder libraryStepsBuilder = stepsBuilderFactory.getLibraryBuilder();
    libraryStepsBuilder.addBuildStepsForLibrary(
        baseJarCommand.getAbiCompatibilityMode(),
        baseJarCommand.getAbiGenerationMode(),
        baseJarCommand.isRequiredForSourceOnlyAbi(),
        baseJarCommand.getTrackClassUsage(),
        baseJarCommand.getBuckOut(),
        baseJarCommand.getBuildTargetValue(),
        baseJarCommand.getCompilerOutputPathsValue(),
        baseJarCommand.getCompileTimeClasspathPaths(),
        baseJarCommand.getCompileTimeClasspathSnapshotPathsMap(),
        baseJarCommand.getJavaSrcs(),
        baseJarCommand.getResourcesMap(),
        baseJarCommand.getJarParameters(),
        baseJarCommand.getBuildCellRootPath(),
        baseJarCommand.getResolvedJavac(),
        actionMetadata,
        kotlinExtraParams,
        kotlinClassesDir);

    libraryStepsBuilder.addMakeMissingOutputsStep(baseJarCommand.getAnnotationPath());

    return libraryStepsBuilder.buildIsolatedSteps();
  }

  private ImmutableList<IsolatedStep> handleAbiJarCommand(
      DefaultCompileStepsBuilderFactory<KotlinExtraParams> stepsBuilderFactory,
      BaseJarCommand baseJarCommand,
      KotlinExtraParams kotlinExtraParams) {
    AbiStepsBuilder abiStepsBuilder = stepsBuilderFactory.getAbiBuilder();

    abiStepsBuilder.addBuildStepsForAbi(
        baseJarCommand.getAbiCompatibilityMode(),
        baseJarCommand.getAbiGenerationMode(),
        baseJarCommand.isRequiredForSourceOnlyAbi(),
        baseJarCommand.getTrackClassUsage(),
        baseJarCommand.getBuckOut(),
        baseJarCommand.getBuildTargetValue(),
        baseJarCommand.getCompilerOutputPathsValue(),
        baseJarCommand.getCompileTimeClasspathPaths(),
        baseJarCommand.getJavaSrcs(),
        baseJarCommand.getResourcesMap(),
        baseJarCommand.getJarParameters(),
        baseJarCommand.getJarParameters(),
        baseJarCommand.getBuildCellRootPath(),
        baseJarCommand.getResolvedJavac(),
        kotlinExtraParams);

    return abiStepsBuilder.buildIsolatedSteps();
  }
}
