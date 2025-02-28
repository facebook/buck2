/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.kotlin.compilerplugins.kosabi.applicability

import com.facebook.kotlin.compilerplugins.kosabi.applicability.common.FEATURE_CHECKERS
import com.facebook.kotlin.compilerplugins.kosabi.applicability.common.checkers.FeatureChecker
import com.facebook.kotlin.compilerplugins.kosabi.applicability.k1.ApplicabilityPluginExtension
import com.facebook.kotlin.compilerplugins.kosabi.applicability.k2.FirApplicabilityExtensionRegistrar
import org.jetbrains.kotlin.cli.common.CLIConfigurationKeys
import org.jetbrains.kotlin.cli.common.messages.CompilerMessageSeverity
import org.jetbrains.kotlin.cli.common.messages.MessageCollector
import org.jetbrains.kotlin.compiler.plugin.CompilerPluginRegistrar
import org.jetbrains.kotlin.config.CompilerConfiguration
import org.jetbrains.kotlin.fir.extensions.FirExtensionRegistrarAdapter
import org.jetbrains.kotlin.resolve.jvm.extensions.AnalysisHandlerExtension

/**
 * Registers [ApplicabilityPluginExtension] to be used by the compiler
 *
 * Note that this needs a resource file
 * META-INF/services/org.jetbrains.kotlin.compiler.plugin.ComponentRegistrar for the compiler to
 * find it and load it
 */
@OptIn(org.jetbrains.kotlin.compiler.plugin.ExperimentalCompilerApi::class)
class ApplicabilityPluginCompilerPluginRegistrar(
    private val checkers: List<FeatureChecker> = FEATURE_CHECKERS
) : CompilerPluginRegistrar() {
  override fun ExtensionStorage.registerExtensions(configuration: CompilerConfiguration) {
    val messageCollector =
        configuration.get(CLIConfigurationKeys.MESSAGE_COLLECTOR_KEY, MessageCollector.NONE)
    messageCollector.report(CompilerMessageSeverity.INFO, "Loaded KosabiApplicabilityPlugin")

    // Create our plugin
    val extension = ApplicabilityPluginExtension(configuration, checkers)

    // Register it to a specific extension point in the compiler, the analysis part
    AnalysisHandlerExtension.registerExtension(extension)

    // Register extension for K2 compiler
    FirExtensionRegistrarAdapter.registerExtension(FirApplicabilityExtensionRegistrar(checkers))
  }

  override val supportsK2: Boolean
    get() = true
}
