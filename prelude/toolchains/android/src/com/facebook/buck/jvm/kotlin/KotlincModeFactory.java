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
import com.facebook.buck.core.util.log.Logger;
import com.facebook.buck.io.file.MostFiles;
import com.facebook.buck.jvm.cd.command.kotlin.KotlinExtraParams;
import com.facebook.buck.jvm.java.ActionMetadata;
import com.facebook.buck.jvm.kotlin.abtesting.ExperimentConfigService;
import com.facebook.buck.jvm.kotlin.abtesting.ksic.KsicExperimentConstantsKt;
import com.facebook.buck.jvm.kotlin.kotlinc.incremental.KotlincMode;
import com.facebook.buck.jvm.kotlin.kotlinc.incremental.RebuildReason;
import com.google.common.collect.ImmutableList;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Optional;
import javax.annotation.Nullable;

public class KotlincModeFactory {
  private static final Logger LOG = Logger.get(KotlincModeFactory.class);

  private ExperimentConfigService experimentConfigService;

  public KotlincModeFactory() {
    this(ExperimentConfigService.loadImplementation());
  }

  public KotlincModeFactory(ExperimentConfigService experimentConfigService) {
    this.experimentConfigService = experimentConfigService;
  }

  public KotlincMode create(
      boolean isSourceOnly,
      final AbsPath rootProjectDir,
      final AbsPath buildDir,
      final boolean isTrackClassUsageEnabled,
      final RelPath kotlinClassUsageFile,
      final KotlinExtraParams extraParams,
      final Optional<ActionMetadata> actionMetadata,
      final ImmutableList<AbsPath> classpathSnapshots) {
    if (!extraParams.getShouldKotlincRunViaBuildToolsApi()) {
      LOG.info("Non-incremental mode applied: kotlinc_run_via_build_tools_api config disabled");
      return KotlincMode.NonIncremental.INSTANCE;
    } else if (!extraParams.getShouldKotlincRunIncrementally()) {
      LOG.info("Non-incremental mode applied: incremental property disabled");
      return KotlincMode.NonIncremental.INSTANCE;
    } else if (isSourceOnly) {
      LOG.info("Non-incremental mode applied: source-only build requested");
      return KotlincMode.NonIncremental.INSTANCE;
    } else {
      AbsPath incrementalStateDir =
          extraParams
              .getIncrementalStateDir()
              .orElseThrow(() -> new IllegalStateException("incremental_state_dir is not created"));

      if (extraParams.getShouldIncrementalKotlicRunQe()
          && !experimentConfigService
              .loadConfig(KsicExperimentConstantsKt.UNIVERSE_NAME)
              .getBoolParam(KsicExperimentConstantsKt.PARAM_KSIC_ENABLED, true)) {
        LOG.info(
            "Non-incremental mode applied: experiment parameter "
                + KsicExperimentConstantsKt.PARAM_KSIC_ENABLED
                + "=false");
        createCleanDirectory(incrementalStateDir);

        return KotlincMode.NonIncremental.INSTANCE;
      }

      @Nullable
      AbsPath kotlinClassUsageFileDir =
          isTrackClassUsageEnabled
              ? incrementalStateDir.resolve(kotlinClassUsageFile.getFileName())
              : null;

      AbsPath kotlicWorkingDir =
          extraParams
              .getKotlincWorkingDir()
              .orElseThrow(
                  () ->
                      new IllegalStateException(
                          "incremental_state_dir/kotlinc_working_dir is not created"));
      ActionMetadata metadata =
          actionMetadata.orElseThrow(
              () -> new IllegalStateException("actionMetadata is not created"));
      LOG.info("Incremental mode applied");

      return new KotlincMode.Incremental(
          rootProjectDir,
          buildDir,
          kotlicWorkingDir,
          KotlinSourceChangesFactory.create(rootProjectDir, metadata),
          ClasspathChangesFactory.create(metadata, classpathSnapshots),
          kotlinClassUsageFileDir,
          checkIfRequiresRebuild(
              kotlinClassUsageFileDir,
              getJvmAbiGenWorkingDir(
                  extraParams.getShouldUseJvmAbiGen(), extraParams.getJvmAbiGenWorkingDir())));
    }
  }

  private static void createCleanDirectory(AbsPath dir) {
    try {
      MostFiles.deleteRecursivelyIfExists(dir.getPath());
      Files.createDirectories(dir.getPath());
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }

  private static @Nullable RebuildReason checkIfRequiresRebuild(
      @Nullable AbsPath kotlinClassUsageFileDir, @Nullable AbsPath jvmAbiGenWorkingDir) {
    if (kotlinClassUsageFileDir != null && !kotlinClassUsageFileDir.toFile().exists()) {
      return new RebuildReason.PreviousKotlinUsedClassesFileNotFound(kotlinClassUsageFileDir);
    }

    if (jvmAbiGenWorkingDir != null && !jvmAbiGenWorkingDir.toFile().exists()) {
      return new RebuildReason.JvmAbiGenWorkingDirNotFound(jvmAbiGenWorkingDir);
    }

    return null;
  }

  private static @Nullable AbsPath getJvmAbiGenWorkingDir(
      boolean shouldUseJvmAbiGen, Optional<AbsPath> jvmAbiGenWorkingDir) {
    if (!shouldUseJvmAbiGen) {
      return null;
    }

    return jvmAbiGenWorkingDir.orElseThrow(
        () -> new IllegalStateException("jvm_abi_gen_working_dir is not created"));
  }
}
