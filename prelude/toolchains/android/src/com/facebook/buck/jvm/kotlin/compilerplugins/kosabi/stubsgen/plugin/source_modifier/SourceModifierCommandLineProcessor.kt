/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.kotlin.compilerplugins.kosabi.stubsgen.plugin.source_modifier

import org.jetbrains.kotlin.compiler.plugin.AbstractCliOption
import org.jetbrains.kotlin.compiler.plugin.CliOption
import org.jetbrains.kotlin.compiler.plugin.CliOptionProcessingException
import org.jetbrains.kotlin.compiler.plugin.CommandLineProcessor
import org.jetbrains.kotlin.config.CompilerConfiguration
import org.jetbrains.kotlin.config.CompilerConfigurationKey

private const val strippedSrcDirName = "stubsgen-stripped-src-dir"
private const val stubsGenLogEnabledName = "stubsgen-log-enabled"

val strippedSrcDirKey: CompilerConfigurationKey<String> =
    CompilerConfigurationKey.create<String>("plugin $strippedSrcDirName")
val stubsGenLogEnabledKey: CompilerConfigurationKey<String> =
    CompilerConfigurationKey.create<String>("plugin $stubsGenLogEnabledName")

@OptIn(org.jetbrains.kotlin.compiler.plugin.ExperimentalCompilerApi::class)
class SourceModifierCommandLineProcessor : CommandLineProcessor {
  override val pluginId: String
    get() = COMPILER_PLUGIN_ID

  override val pluginOptions: Collection<AbstractCliOption>
    get() = listOf(LOG_ENABLED, STRIPPED_SRC_DIR)

  override fun processOption(
      option: AbstractCliOption,
      value: String,
      configuration: CompilerConfiguration,
  ) {
    when (option.optionName) {
      strippedSrcDirName -> configuration.put(strippedSrcDirKey, value)
      stubsGenLogEnabledName -> configuration.put(stubsGenLogEnabledKey, value)
      else -> throw CliOptionProcessingException("Unknown option: ${option.optionName}")
    }
  }

  companion object {
    const val COMPILER_PLUGIN_ID: String =
        "com.facebook.kotlin.compilerplugins.kosabi.source_modifier"

    val STRIPPED_SRC_DIR: CliOption =
        CliOption(
            optionName = strippedSrcDirName,
            valueDescription = "<file-path>",
            description = "Path to stripped output directory.",
            required = false,
            allowMultipleOccurrences = false,
        )
    val LOG_ENABLED: CliOption =
        CliOption(
            optionName = stubsGenLogEnabledName,
            valueDescription = "<file-path>",
            description = "Enable logger to write logs in file. local only",
            required = false,
            allowMultipleOccurrences = false,
        )
  }
}
