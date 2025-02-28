/*
 * Portions Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

/*
 * Copyright 2010-2018 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package com.facebook

import org.jetbrains.kotlin.config.CompilerConfigurationKey

@OptIn(org.jetbrains.kotlin.compiler.plugin.ExperimentalCompilerApi::class)
@SuppressWarnings("PackageLocationMismatch")
object JvmAbiConfigurationKeys {
  val OUTPUT_PATH: CompilerConfigurationKey<String> =
      CompilerConfigurationKey.create<String>(
          JvmAbiCommandLineProcessor.OUTPUT_PATH_OPTION.description)

  val LEGACY_ABI_GEN: CompilerConfigurationKey<Boolean> =
      CompilerConfigurationKey.create<Boolean>(
          JvmAbiCommandLineProcessor.LEGACY_ABI_GEN_OPTION.description)

  val EARLY_TERMINATION: CompilerConfigurationKey<Boolean> =
      CompilerConfigurationKey.create<Boolean>(
          JvmAbiCommandLineProcessor.EARLY_TERMINATION_OPTION.description)

  val ENABLE_MIXED_COMPILATION: CompilerConfigurationKey<Boolean> =
      CompilerConfigurationKey.create<Boolean>(
          JvmAbiCommandLineProcessor.ENABLE_MIXED_COMPILATION.description)

  val DEBUG_JAVA_KSTUB_OUTPUT_PATH: CompilerConfigurationKey<String> =
      CompilerConfigurationKey.create<String>(
          JvmAbiCommandLineProcessor.DEBUG_JAVA_KSTUB_PATH_OPTION.description)
}
