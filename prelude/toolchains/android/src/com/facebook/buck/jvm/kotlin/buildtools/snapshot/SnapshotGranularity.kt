/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.kotlin.buildtools.snapshot

import org.jetbrains.kotlin.buildtools.api.jvm.ClassSnapshotGranularity

enum class SnapshotGranularity {
  CLASS_LEVEL,
  CLASS_MEMBER_LEVEL;

  val toClassSnapshotGranularity: ClassSnapshotGranularity
    get() =
        when (this) {
          CLASS_LEVEL -> ClassSnapshotGranularity.CLASS_LEVEL
          CLASS_MEMBER_LEVEL -> ClassSnapshotGranularity.CLASS_MEMBER_LEVEL
        }
}
