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
      @Nullable AbsPath depFile,
      @Nullable AbsPath usedJars,
      @Nullable AbsPath jvmAbiGenDir) {
    if (actionMetadata.getPreviousIncrementalMetadataDigest() == null) {
      return RebuildReason.NO_LAST_BUILD_CONFIGURATION;
    }

    if (!actionMetadata
        .getPreviousIncrementalMetadataDigest()
        .equals(actionMetadata.getCurrentIncrementalMetadataDigest())) {
      return RebuildReason.BUILD_CONFIGURATION_CHANGED;
    }

    if (depFile != null && !depFile.toFile().exists()) {
      return RebuildReason.NO_LAST_DEP_FILE;
    }

    if (usedJars != null && !usedJars.toFile().exists()) {
      return RebuildReason.NO_LAST_USED_JARS;
    }

    if (jvmAbiGenDir != null && !jvmAbiGenDir.toFile().exists()) {
      return RebuildReason.NO_JVM_ABI_DIR;
    }

    return null;
  }
}
