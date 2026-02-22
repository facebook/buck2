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

package com.facebook.kotlin.compilerplugins.kosabi.stubsgen.plugin.stubsgen

import com.facebook.kotlin.compilerplugins.kosabi.common.Logger
import java.io.File
import org.jetbrains.kotlin.cli.jvm.config.jvmClasspathRoots
import org.jetbrains.kotlin.com.intellij.mock.MockProject
import org.jetbrains.kotlin.com.intellij.openapi.extensions.LoadingOrder
import org.jetbrains.kotlin.compiler.plugin.ComponentRegistrar
import org.jetbrains.kotlin.config.CompilerConfiguration
import org.jetbrains.kotlin.extensions.CollectAdditionalSourcesExtension

@OptIn(org.jetbrains.kotlin.compiler.plugin.ExperimentalCompilerApi::class)
class StubsCodegenComponentRegistrar : ComponentRegistrar {
  override fun registerProjectComponents(
      project: MockProject,
      configuration: CompilerConfiguration,
  ) {
    val stubsGenDir: File? = configuration.get(stubsGenDirKey)?.let { File(it) }
    val stubsClassOutputDir: File? = configuration.get(stubsClassOutputDirKey)?.let { File(it) }

    // Register logger path if set
    Logger.userDefinedPath = configuration.get(stubsGenLogEnabledKey)
    val classPaths = configuration.jvmClasspathRoots
    Logger.log("[ClassPaths]\n")
    classPaths.forEach { Logger.log(it.canonicalPath) }
    // We're generation stubs after all additional sources were collected
    project.extensionArea
        .getExtensionPoint(CollectAdditionalSourcesExtension.extensionPointName)
        .registerExtension(
            StubsAdditionalSourcesExtension(
                stubsDumpDir = stubsGenDir,
                stubsClassOutputDir = stubsClassOutputDir,
                classPaths = classPaths,
            ),
            LoadingOrder.LAST,
            project,
        )
  }
}
