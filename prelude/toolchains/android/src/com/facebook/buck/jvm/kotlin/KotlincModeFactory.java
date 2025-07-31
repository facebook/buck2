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
import com.facebook.buck.core.util.log.Logger;
import com.facebook.buck.jvm.cd.command.kotlin.KotlinExtraParams;
import com.facebook.buck.jvm.java.ActionMetadata;
import com.facebook.buck.jvm.kotlin.kotlinc.incremental.KotlincMode;
import com.google.common.collect.ImmutableList;
import java.util.Optional;
import javax.annotation.Nullable;

public class KotlincModeFactory {
  private static final Logger LOG = Logger.get(KotlincModeFactory.class);

  private IncrementalCompilationValidator incrementalCompilationValidator;

  public KotlincModeFactory() {
    this(new IncrementalCompilationValidator());
  }

  public KotlincModeFactory(IncrementalCompilationValidator incrementalCompilationValidator) {
    this.incrementalCompilationValidator = incrementalCompilationValidator;
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
    if (!extraParams.getShouldKotlincRunIncrementally()) {
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
          KotlinSourceChangesFactory.create(
              rootProjectDir, new SourceFilesActionMetadata(metadata)),
          ClasspathChangesFactory.create(new JarsActionMetadata(metadata), classpathSnapshots),
          kotlinClassUsageFileDir,
          incrementalCompilationValidator.validate(
              metadata,
              kotlinClassUsageFileDir,
              getJvmAbiGenWorkingDir(
                  extraParams.getShouldUseJvmAbiGen(), extraParams.getJvmAbiGenWorkingDir())));
    }
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
