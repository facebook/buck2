/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook

import org.jetbrains.kotlin.config.CompilerConfigurationKey

@SuppressWarnings("PackageLocationMismatch")
object K2JvmAbiConfigurationKeys {
  val OUTPUT_PATH: CompilerConfigurationKey<String> =
      CompilerConfigurationKey.create("output path for K2 JVM ABI generation")

  val EARLY_TERMINATION: CompilerConfigurationKey<Boolean> =
      CompilerConfigurationKey.create("early termination flag for K2 JVM ABI generation")

  val ENABLE_MIXED_COMPILATION: CompilerConfigurationKey<Boolean> =
      CompilerConfigurationKey.create("enable mixed compilation for K2 JVM ABI generation")

  val DEBUG_OUTPUT: CompilerConfigurationKey<Boolean> =
      CompilerConfigurationKey.create("debug output flag for K2 JVM ABI generation")
}
