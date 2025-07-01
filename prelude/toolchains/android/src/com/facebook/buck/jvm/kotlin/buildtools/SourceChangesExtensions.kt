/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
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
