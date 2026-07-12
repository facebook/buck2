/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.kotlin.compilerplugins.kosabi.stubsgen.plugin.stubsgen_k2

import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.generators.StubsGenAPI // @oss-enable
import com.facebook.kotlin.compilercompat.FirAnalysisHandlerExtensionCompat
// @oss-disable: import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.generators.meta_only.StubsGenApiImpl
import java.io.File
import org.jetbrains.kotlin.cli.common.config.kotlinSourceRoots
import org.jetbrains.kotlin.cli.jvm.compiler.createSourceFilesFromSourceRoots
import org.jetbrains.kotlin.com.intellij.openapi.project.Project
import org.jetbrains.kotlin.config.CompilerConfiguration

@SuppressWarnings("PackageLocationMismatch")
class StubsCodegenK2FirAnalysisHandlerExtension(
    private val stubsDumpDir: File,
    private val stubsClassOutputDir: File,
    private val classPaths: List<File>,
) : FirAnalysisHandlerExtensionCompat() {

  override fun isApplicable(configuration: CompilerConfiguration): Boolean {
    return true
  }

  override fun doAnalysis(project: Project, configuration: CompilerConfiguration): Boolean {
    try {
      // Use the compiler's canonical source collection, which recognizes Kotlin sources by file
      // type (including .kts) and reports an error for missing source roots. Mirrors the approach
      // used by K2JvmAbiFirAnalysisHandlerExtension and replaces the deprecated
      // KotlinCoreEnvironment-based K1 source lookup.
      val ktFiles =
          createSourceFilesFromSourceRoots(configuration, project, configuration.kotlinSourceRoots)
      StubsGenAPI( // @oss-enable
      // @oss-disable: StubsGenApiImpl(
          stubsDumpDir,
          stubsClassOutputDir,
          classPaths,
      )
          .generateStubs(ktFiles, configuration, project)
    } finally {
      throw RuntimeException("Terminating compilation. We're done with Stubgen.")
    }

    return true
  }
}
