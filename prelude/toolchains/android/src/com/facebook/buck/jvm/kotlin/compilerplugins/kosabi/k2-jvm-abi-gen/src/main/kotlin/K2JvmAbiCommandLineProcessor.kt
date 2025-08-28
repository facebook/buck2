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

import org.jetbrains.kotlin.compiler.plugin.AbstractCliOption
import org.jetbrains.kotlin.compiler.plugin.CliOption
import org.jetbrains.kotlin.compiler.plugin.CliOptionProcessingException
import org.jetbrains.kotlin.compiler.plugin.CommandLineProcessor
import org.jetbrains.kotlin.config.CompilerConfiguration

@OptIn(org.jetbrains.kotlin.compiler.plugin.ExperimentalCompilerApi::class)
@SuppressWarnings("PackageLocationMismatch")
class K2JvmAbiCommandLineProcessor : CommandLineProcessor {
  companion object {
    const val COMPILER_PLUGIN_ID: String = "com.facebook.k2.jvm.abi.gen"

    val OUTPUT_PATH_OPTION: CliOption =
        CliOption(
            "outputDir",
            "<path>",
            "Output path for generated files. This can be either a directory or a jar file.",
            true,
        )

    val EARLY_TERMINATION_OPTION: CliOption =
        CliOption(
            "earlyTermination",
            "<boolean>",
            "Whether we should terminate K2JvmAbiGen early. Default is false.",
            required = false,
        )

    val ENABLE_MIXED_COMPILATION: CliOption =
        CliOption(
            "enableMixedCompilation",
            "<boolean>",
            "Enable to translate Java sources into ABI. Default is false.",
            false,
        )

    val DEBUG_OUTPUT_OPTION: CliOption =
        CliOption(
            "debugOutput",
            "<boolean>",
            "Enable debug output for K2 ABI generation. Default is false.",
            false,
        )
  }

  override val pluginId: String
    get() = COMPILER_PLUGIN_ID

  override val pluginOptions: Collection<CliOption>
    get() =
        listOf(
            OUTPUT_PATH_OPTION,
            EARLY_TERMINATION_OPTION,
            ENABLE_MIXED_COMPILATION,
            DEBUG_OUTPUT_OPTION,
        )

  override fun processOption(
      option: AbstractCliOption,
      value: String,
      configuration: CompilerConfiguration,
  ) {
    when (option) {
      OUTPUT_PATH_OPTION -> configuration.put(K2JvmAbiConfigurationKeys.OUTPUT_PATH, value)
      EARLY_TERMINATION_OPTION ->
          configuration.put(K2JvmAbiConfigurationKeys.EARLY_TERMINATION, value == "true")
      ENABLE_MIXED_COMPILATION ->
          configuration.put(K2JvmAbiConfigurationKeys.ENABLE_MIXED_COMPILATION, value == "true")
      DEBUG_OUTPUT_OPTION ->
          configuration.put(K2JvmAbiConfigurationKeys.DEBUG_OUTPUT, value == "true")
      else -> throw CliOptionProcessingException("Unknown option: ${option.optionName}")
    }
  }
}
