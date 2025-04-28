/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.kotlin.cd.analytics

enum class StepParam(val value: String) {
  KOTLINC("kotlinc"),
  KOSABI_STUBGEN("kosabi_stubgen"),
  KSP1("ksp1"),
  KSP2("ksp2"),
  KAPT("kapt"),
}

sealed class KotlincModeParam(val value: String) {

  data class Incremental(val classpathChangesParam: ClasspathChangesParam) :
      KotlincModeParam("incremental")

  data object NonIncremental : KotlincModeParam("non_incremental")
}

enum class ClasspathChangesParam(val value: String) {
  TO_BE_COMPUTED_BY_INCREMENTAL_COMPILER("to_be_computed_by_incremental_compiler"),
  NO_CHANGES("no_changes"),
  UNKNOWN("unknown")
}
