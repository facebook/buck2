/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.kotlin.compilerplugins.kosabi.applicability.common

import com.facebook.kotlin.compilerplugins.kosabi.applicability.common.checkers.Checker
import kotlin.RuntimeException

class KotlinSourceOnlyAbiApplicabilityException(checkers: Map<Checker, Set<String>>) :
    RuntimeException(composeErrorMessage(checkers))

private fun composeErrorMessage(checkers: Map<Checker, Set<String>>): String = buildString {
  appendLine(
      """
          |
          | ===========================================
          |Kosabi/Applicability FAILED on this target.
          |
          |Kosabi/Applicability verifies that it's possible to build a source-only-abi of the target.
          |This target has [abi_generation_mode = "source_only"].
          |
          |How to fix:
          | 1. [Preferred] Fix source-only-abi violations in the list below.
          | 2. [Try to avoid. Will DEGRADE build speed] Opt-out removing [abi_generation_mode = "source_only"] in your target.
          |
          | Source-only-abi violations list:
          """
          .trimMargin())

  checkers.forEach { (checker, detectedFiles) ->
    appendLine("--- Failed ${checker.name} ---")
    appendLine(
        """
            |What happened?
            |  ${checker.description}
            """
            .trimMargin())

    appendLine(
        """
            |How to fix?
            |  ${checker.fix.type} fix available: ${checker.fix.howToFix}
            """
            .trimMargin())

    detectedFiles.forEach { appendLine(it) }
    appendLine("---")
  }

  appendLine(
      """
          |
          |Read More: https://fburl.com/kosabi
          |Questions: https://fburl.com/kotlinfoundationgroup
          | ===========================================
          """
          .trimMargin())
}
