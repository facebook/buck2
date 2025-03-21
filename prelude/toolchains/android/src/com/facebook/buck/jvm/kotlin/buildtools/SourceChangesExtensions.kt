/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

@file:OptIn(ExperimentalBuildToolsApi::class)

package com.facebook.buck.jvm.kotlin.buildtools

import com.facebook.buck.jvm.kotlin.kotlinc.incremental.KotlinSourceChanges
import org.jetbrains.kotlin.buildtools.api.ExperimentalBuildToolsApi
import org.jetbrains.kotlin.buildtools.api.SourcesChanges

internal fun KotlinSourceChanges.toSourcesChanges(): SourcesChanges =
    when (this) {
      is KotlinSourceChanges.ToBeCalculated -> {
        SourcesChanges.ToBeCalculated
      }
      is KotlinSourceChanges.Known -> {
        SourcesChanges.Known(
            this.addedAndModifiedFiles.map { path -> path.toFile() },
            this.removedFiles.map { it.toFile() })
      }
    }
