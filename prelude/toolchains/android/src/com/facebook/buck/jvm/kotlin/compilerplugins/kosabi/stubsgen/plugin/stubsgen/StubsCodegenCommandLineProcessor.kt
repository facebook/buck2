/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.kotlin.compilerplugins.kosabi.stubsgen.plugin.stubsgen

import org.jetbrains.kotlin.compiler.plugin.AbstractCliOption
import org.jetbrains.kotlin.compiler.plugin.CliOption
import org.jetbrains.kotlin.compiler.plugin.CliOptionProcessingException
import org.jetbrains.kotlin.compiler.plugin.CommandLineProcessor
import org.jetbrains.kotlin.config.CompilerConfiguration
import org.jetbrains.kotlin.config.CompilerConfigurationKey

private const val stubsGenDirName = "stubsgen-dir"
private const val stubsGenLogEnabledName = "stubsgen-log-enabled"
private const val stubsGenStandaloneModeName = "stubsgen-standalone-mode"

val stubsGenDirKey: CompilerConfigurationKey<String> =
    CompilerConfigurationKey.create<String>("plugin $stubsGenDirName")
val stubsGenLogEnabledKey: CompilerConfigurationKey<String> =
    CompilerConfigurationKey.create<String>("plugin $stubsGenLogEnabledName")
val stubsGenStandaloneModeKey: CompilerConfigurationKey<Boolean> =
    CompilerConfigurationKey.create<Boolean>("plugin $stubsGenStandaloneModeName")

@OptIn(org.jetbrains.kotlin.compiler.plugin.ExperimentalCompilerApi::class)
class StubsCodegenCommandLineProcessor : CommandLineProcessor {
  override val pluginId: String
    get() = COMPILER_PLUGIN_ID

  override val pluginOptions: Collection<AbstractCliOption>
    get() = listOf(STUBS_GEN_DIR, LOG_ENABLED, STANDALONE_MODE)

  override fun processOption(
      option: AbstractCliOption,
      value: String,
      configuration: CompilerConfiguration
  ) {
    when (option.optionName) {
      stubsGenDirName -> configuration.put(stubsGenDirKey, value)
      stubsGenLogEnabledName -> configuration.put(stubsGenLogEnabledKey, value)
      stubsGenStandaloneModeName -> configuration.put(stubsGenStandaloneModeKey, value == "true")
      else -> throw CliOptionProcessingException("Unknown option: ${option.optionName}")
    }
  }

  companion object {
    const val COMPILER_PLUGIN_ID: String = "com.facebook.kotlin.compilerplugins.kosabi.stubsgen"

    val STUBS_GEN_DIR: CliOption =
        CliOption(
            optionName = stubsGenDirName,
            valueDescription = "<file-path>",
            description = "Path to stubs directory. test-only",
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
    val STANDALONE_MODE: CliOption =
        CliOption(
            optionName = stubsGenStandaloneModeName,
            valueDescription = "<boolean>",
            description =
                "Run stubs generation as a standalone mode plugin, which will terminate early and output the stubs in compatible filename format for KSP",
            required = false,
        )
  }
}
