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

import org.jetbrains.kotlin.compiler.plugin.AbstractCliOption
import org.jetbrains.kotlin.compiler.plugin.CliOption
import org.jetbrains.kotlin.compiler.plugin.CliOptionProcessingException
import org.jetbrains.kotlin.compiler.plugin.CommandLineProcessor
import org.jetbrains.kotlin.config.CompilerConfiguration

@OptIn(org.jetbrains.kotlin.compiler.plugin.ExperimentalCompilerApi::class)
@SuppressWarnings("PackageLocationMismatch")
class JvmAbiCommandLineProcessor : CommandLineProcessor {
  companion object {
    const val COMPILER_PLUGIN_ID: String = "com.facebook.jvm.abi.gen"

    val OUTPUT_PATH_OPTION: CliOption =
        CliOption(
            "outputDir",
            "<path>",
            "Output path for generated files. This can be either a directory or a jar file.",
            true)

    val LEGACY_ABI_GEN_OPTION: CliOption =
        CliOption(
            "useLegacyAbiGen",
            "true|false",
            "Use the legacy two pass implementation of jvm-abi-gen.",
            false)

    val EARLY_TERMINATION_OPTION: CliOption =
        CliOption(
            "earlyTermination",
            "<boolean>",
            "Whether we should terminate JvmAbiGen early. Default is false.",
            required = false)

    val ENABLE_MIXED_COMPILATION: CliOption =
        CliOption(
            "enableMixedCompilation",
            "<boolean>",
            "Enable to translate Java sources into ABI. Default is false.",
            false)

    val DEBUG_JAVA_KSTUB_PATH_OPTION: CliOption =
        CliOption(
            "debugJavaKStubOutputDir",
            "<path>",
            "Java converted KStub output directory path. Debug only.",
            false)
  }

  override val pluginId: String
    get() = COMPILER_PLUGIN_ID

  override val pluginOptions: Collection<CliOption>
    get() =
        listOf(
            OUTPUT_PATH_OPTION,
            LEGACY_ABI_GEN_OPTION,
            EARLY_TERMINATION_OPTION,
            ENABLE_MIXED_COMPILATION,
            DEBUG_JAVA_KSTUB_PATH_OPTION)

  override fun processOption(
      option: AbstractCliOption,
      value: String,
      configuration: CompilerConfiguration
  ) {
    when (option) {
      OUTPUT_PATH_OPTION -> configuration.put(JvmAbiConfigurationKeys.OUTPUT_PATH, value)
      LEGACY_ABI_GEN_OPTION ->
          configuration.put(JvmAbiConfigurationKeys.LEGACY_ABI_GEN, value == "true")
      EARLY_TERMINATION_OPTION ->
          configuration.put(JvmAbiConfigurationKeys.EARLY_TERMINATION, value == "true")
      ENABLE_MIXED_COMPILATION ->
          configuration.put(JvmAbiConfigurationKeys.ENABLE_MIXED_COMPILATION, value == "true")
      DEBUG_JAVA_KSTUB_PATH_OPTION ->
          configuration.put(JvmAbiConfigurationKeys.DEBUG_JAVA_KSTUB_OUTPUT_PATH, value)
      else -> throw CliOptionProcessingException("Unknown option: ${option.optionName}")
    }
  }
}
