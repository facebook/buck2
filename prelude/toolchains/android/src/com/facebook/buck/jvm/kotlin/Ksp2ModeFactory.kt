/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

@file:JvmName("Ksp2ModeFactory")

package com.facebook.buck.jvm.kotlin

import com.facebook.buck.core.filesystems.RelPath
import com.facebook.buck.core.util.log.Logger
import com.facebook.buck.jvm.cd.command.kotlin.KotlinExtraParams
import com.facebook.buck.jvm.java.ActionMetadata
import com.facebook.buck.jvm.kotlin.ksp.incremental.Ksp2Mode

@JvmName("create")
fun Ksp2Mode(
    isSourceOnly: Boolean,
    kspCachesOutput: RelPath,
    extraParams: KotlinExtraParams,
    actionMetadata: ActionMetadata?
): Ksp2Mode {
  when {
    !extraParams.shouldKsp2RunIncrementally -> {
      LOG.info("Non-incremental mode applied: incremental property disabled")
      return Ksp2Mode.NonIncremental(kspCachesOutput)
    }
    isSourceOnly -> {
      LOG.info("Non-incremental mode applied: source-only build requested")
      return Ksp2Mode.NonIncremental(kspCachesOutput)
    }
    else -> {
      val cachesDir =
          extraParams.ksp2CachesDir.orElseThrow {
            IllegalStateException("incremental_state_dir/ksp2_caches_dir is not created")
          }
      val metadata = SourceFilesActionMetadata(requireNotNull(actionMetadata))

      LOG.info("Incremental mode applied")

      return Ksp2Mode.Incremental(cachesDir, true, emptyList(), emptyList(), emptyList())
    }
  }
}

private val LOG = Logger.get("Ksp2ModeFactory")
