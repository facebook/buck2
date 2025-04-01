/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.kotlin.kotlinc.incremental

import com.facebook.buck.core.filesystems.AbsPath

sealed interface RebuildReason {

  val reason: String

  data class PreviousKotlinUsedClassesFileNotFound(private val kotlinClassUsageFileDir: AbsPath) :
      RebuildReason {

    override val reason: String = "${kotlinClassUsageFileDir.fileName} not found"
  }

  data class JvmAbiGenWorkingDirNotFound(private val jvmAbiGenWorkingDir: AbsPath) : RebuildReason {

    override val reason: String = "${jvmAbiGenWorkingDir.fileName} not found"
  }
}
