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
import com.facebook.buck.jvm.java.ActionMetadata;
import com.facebook.buck.jvm.kotlin.kotlinc.incremental.RebuildReason;
import javax.annotation.Nullable;

public class IncrementalCompilationValidator {

  public IncrementalCompilationValidator() {}

  public @Nullable RebuildReason validate(
      ActionMetadata actionMetadata,
      @Nullable AbsPath kotlinClassUsageFileDir,
      @Nullable AbsPath jvmAbiGenWorkingDir) {
    if (actionMetadata.getPreviousIncrementalMetadataDigest() == null) {
      return RebuildReason.PreviousBuildConfigurationNotFound.INSTANCE;
    }

    if (!actionMetadata
        .getPreviousIncrementalMetadataDigest()
        .equals(actionMetadata.getCurrentIncrementalMetadataDigest())) {
      return RebuildReason.BuildConfigurationChanged.INSTANCE;
    }

    if (kotlinClassUsageFileDir != null && !kotlinClassUsageFileDir.toFile().exists()) {
      return new RebuildReason.PreviousKotlinUsedClassesFileNotFound(kotlinClassUsageFileDir);
    }

    if (jvmAbiGenWorkingDir != null && !jvmAbiGenWorkingDir.toFile().exists()) {
      return new RebuildReason.JvmAbiGenWorkingDirNotFound(jvmAbiGenWorkingDir);
    }

    return null;
  }
}
