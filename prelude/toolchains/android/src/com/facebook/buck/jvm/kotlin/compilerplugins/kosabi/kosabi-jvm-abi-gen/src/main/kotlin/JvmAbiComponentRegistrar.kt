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

import java.io.File
import org.jetbrains.kotlin.com.intellij.mock.MockProject
import org.jetbrains.kotlin.com.intellij.openapi.extensions.LoadingOrder
import org.jetbrains.kotlin.compiler.plugin.ComponentRegistrar
import org.jetbrains.kotlin.config.CompilerConfiguration
import org.jetbrains.kotlin.config.JVMConfigurationKeys
import org.jetbrains.kotlin.resolve.jvm.extensions.AnalysisHandlerExtension

@OptIn(org.jetbrains.kotlin.compiler.plugin.ExperimentalCompilerApi::class)
@SuppressWarnings("PackageLocationMismatch")
class JvmAbiComponentRegistrar : ComponentRegistrar {
  override fun registerProjectComponents(
      project: MockProject,
      configuration: CompilerConfiguration
  ) {
    val outputPath = configuration.getNotNull(JvmAbiConfigurationKeys.OUTPUT_PATH)
    if (configuration.get(JvmAbiConfigurationKeys.LEGACY_ABI_GEN, false)) {
      // Use the two-pass implementation
      require(!outputPath.endsWith(".jar")) { "Legacy jvm-abi-gen does not support jar output." }
      val extension =
          JvmAbiAnalysisHandlerExtension(
              configuration.copy().apply {
                put(JVMConfigurationKeys.OUTPUT_DIRECTORY, File(outputPath))
              })

      // Change jvm-abi-gen to the last order of compiler plugins
      val analysisHandlerExtensionPoint =
          project.extensionArea.getExtensionPoint(AnalysisHandlerExtension.extensionPointName)
      analysisHandlerExtensionPoint.registerExtension(extension, LoadingOrder.LAST, project)
    } else {
      throw RuntimeException("Kosabi not expected to reach the non-legacy branch of JvmAbiGen")
    }
  }

  override val supportsK2: Boolean
    get() = true
}
