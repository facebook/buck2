/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.kotlin.compilerplugins.kosabi.applicability.common

import com.facebook.kotlin.compilerplugins.dataclassgenerate.annotation.DataClassGenerate

enum class CheckerFixType {
  /** Fixes with a manual workaround. */
  MANUAL,

  /** Fixes that could be removed with an automatic codemod. */
  CODEMOD,

  /** Fixes without workarounds. Like DI usage. */
  NOT_AVAILABLE
}

@DataClassGenerate data class CheckerFix(val type: CheckerFixType, val howToFix: String)
