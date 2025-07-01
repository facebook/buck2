/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.kotlin.kotlinc.incremental

import com.facebook.buck.core.filesystems.AbsPath

sealed interface KotlincMode {

  data object NonIncremental : KotlincMode

  data class Incremental(
      val rootProjectDir: AbsPath,
      val buildDir: AbsPath,
      val kotlicWorkingDir: AbsPath,
      val kotlinSourceChanges: KotlinSourceChanges,
      val classpathChanges: ClasspathChanges,
      val kotlinClassUsageFile: AbsPath?,
      val rebuildReason: RebuildReason?
  ) : KotlincMode
}
