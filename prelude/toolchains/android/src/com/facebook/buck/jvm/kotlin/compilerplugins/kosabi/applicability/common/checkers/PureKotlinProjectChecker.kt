/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.kotlin.compilerplugins.kosabi.applicability.common.checkers

import com.facebook.kotlin.compilerplugins.kosabi.applicability.common.CheckerFix
import com.facebook.kotlin.compilerplugins.kosabi.applicability.common.CheckerFixType
import com.facebook.kotlin.compilerplugins.kosabi.applicability.common.config.Config
import org.jetbrains.kotlin.cli.common.config.kotlinSourceRoots
import org.jetbrains.kotlin.cli.jvm.config.javaSourceRoots
import org.jetbrains.kotlin.config.CompilerConfiguration

/** Verifies that target is pure Kotlin. */
object PureKotlinProjectChecker : ProjectChecker {
  override val name: String = "Pure Kotlin project"
  override val description: String =
      "Kosabi is applicable to pure-Kotlin modules only. This module is not pure-Kotlin."
  override val fix: CheckerFix =
      CheckerFix(
          CheckerFixType.MANUAL,
          "You have to convert the remaining parts of this module to Kotlin to adopt source-only-abi.")

  override fun findViolations(configuration: CompilerConfiguration): List<String> {
    // [javaSourceRoots] - raw source roots passed to Kotlinc
    // [kotlinSourceRoots] - parsed only-Kotlin roots
    //
    // If they are equivalent - we're compiling a pure Kotlin target
    // We tolerate the difference in some cases, for example if KAPT created some sources
    //
    // TODO: Find a way to path generated sources to Kosabi/Applicability
    val nonKotlinSourceRoots =
        configuration.javaSourceRoots - configuration.kotlinSourceRoots.map { it.path }.toSet()
    val extraRoots =
        nonKotlinSourceRoots.filter { root ->
          Config.tolerateAdditionalJavaSourcePath.none { root.contains(it) }
        }

    return extraRoots
  }
}
