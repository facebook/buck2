/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

@file:Suppress("DEPRECATION_ERROR")

package com.facebook.kotlin.compilerplugins.kosabi.stubsgen.plugin.source_modifier

import com.facebook.kotlin.compilerplugins.kosabi.common.Logger
import java.io.File
import org.jetbrains.kotlin.cli.jvm.config.jvmClasspathRoots
import org.jetbrains.kotlin.com.intellij.mock.MockProject
import org.jetbrains.kotlin.com.intellij.openapi.extensions.LoadingOrder
import org.jetbrains.kotlin.compiler.plugin.ComponentRegistrar
import org.jetbrains.kotlin.config.CompilerConfiguration
import org.jetbrains.kotlin.extensions.ProcessSourcesBeforeCompilingExtension

@OptIn(org.jetbrains.kotlin.compiler.plugin.ExperimentalCompilerApi::class)
class SourceModifierComponentRegistrar : ComponentRegistrar {
  override fun registerProjectComponents(
      project: MockProject,
      configuration: CompilerConfiguration,
  ) {
    val strippedSrcDir: File? = configuration.get(strippedSrcDirKey)?.let { File(it) }
    // Register logger path if set
    Logger.userDefinedPath = configuration.get(stubsGenLogEnabledKey)
    val classPaths = configuration.jvmClasspathRoots
    Logger.log("[ClassPaths]\n")
    classPaths.forEach { Logger.log(it.canonicalPath) }

    // [SourceModifierExtension]
    // To make sure Kosabi/stubsgen not affecting other plugins doing analysis, we need to register
    // at last order. We have KotlinDI plugins which requires data collection to generate Metadata.
    project.extensionArea
        .getExtensionPoint(ProcessSourcesBeforeCompilingExtension.extensionPointName)
        .registerExtension(SourceModifierExtension(strippedSrcDir), LoadingOrder.LAST, project)
  }
}
