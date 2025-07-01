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
      return RebuildReason.NO_LAST_BUILD_CONFIGURATION;
    }

    if (!actionMetadata
        .getPreviousIncrementalMetadataDigest()
        .equals(actionMetadata.getCurrentIncrementalMetadataDigest())) {
      return RebuildReason.BUILD_CONFIGURATION_CHANGED;
    }

    if (kotlinClassUsageFileDir != null && !kotlinClassUsageFileDir.toFile().exists()) {
      return RebuildReason.NO_LAST_KOTLIN_USED_CLASSES_FILE;
    }

    if (jvmAbiGenWorkingDir != null && !jvmAbiGenWorkingDir.toFile().exists()) {
      return RebuildReason.NO_JVM_ABI_WORKING_DIR;
    }

    return null;
  }
}
