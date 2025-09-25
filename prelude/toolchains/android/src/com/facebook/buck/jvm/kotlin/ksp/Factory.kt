/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.kotlin.ksp

import com.facebook.buck.jvm.cd.command.kotlin.LanguageVersion
import com.facebook.buck.jvm.kotlin.cd.analytics.ClasspathChangesParam
import com.facebook.buck.jvm.kotlin.cd.analytics.KotlinCDLoggingContext
import com.facebook.buck.jvm.kotlin.cd.analytics.ModeParam
import com.facebook.buck.jvm.kotlin.cd.analytics.StepParam
import com.facebook.buck.jvm.kotlin.ksp.incremental.Ksp2Mode

internal fun KotlinCDLoggingContext(
    languageVersion: LanguageVersion,
    ksp2Mode: Ksp2Mode,
): KotlinCDLoggingContext =
    KotlinCDLoggingContext(
            step = StepParam.KSP2,
            languageVersion = languageVersion,
            mode = ModeParam(ksp2Mode),
        )
        .apply {
          (ksp2Mode as? Ksp2Mode.Incremental)?.reprocessReason?.message?.let { message ->
            addExtras(
                "KotlinCDLoggingContextFactory",
                "Non-incremental processing will be performed: $message",
            )
          }
        }

private fun ModeParam(ksp2Mode: Ksp2Mode) =
    when (ksp2Mode) {
      is Ksp2Mode.NonIncremental -> ModeParam.NonIncremental
      is Ksp2Mode.Incremental -> {
        ModeParam.Incremental(
            ClasspathChangesParam.UNKNOWN,
            ksp2Mode.modifiedSources.toSet(),
            ksp2Mode.removedSources.toSet(),
        )
      }
    }
