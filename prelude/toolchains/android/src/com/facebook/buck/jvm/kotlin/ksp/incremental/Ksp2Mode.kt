/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.kotlin.ksp.incremental

import com.facebook.buck.core.filesystems.AbsPath
import com.facebook.buck.core.filesystems.RelPath

sealed interface Ksp2Mode {

  data class NonIncremental(val kspCachesOutput: RelPath) : Ksp2Mode

  data class Incremental(
      val cachesDir: AbsPath,
      val incrementalLog: Boolean,
      val modifiedSources: List<AbsPath>,
      val removedSources: List<AbsPath>,
      val changedClasses: List<String>,
      val reprocessReason: ReprocessReason?,
  ) : Ksp2Mode
}
