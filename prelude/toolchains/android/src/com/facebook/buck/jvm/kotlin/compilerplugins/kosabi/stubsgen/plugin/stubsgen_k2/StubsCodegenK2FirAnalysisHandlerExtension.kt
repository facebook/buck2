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
import kotlin.collections.component1
import kotlin.collections.component2
import org.jetbrains.kotlin.analysis.api.standalone.buildStandaloneAnalysisAPISession
import org.jetbrains.kotlin.com.intellij.openapi.project.Project
import org.jetbrains.kotlin.com.intellij.openapi.util.Disposer
import org.jetbrains.kotlin.config.CompilerConfiguration
import org.jetbrains.kotlin.psi.KtFile

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

    val projectDisposable = Disposer.newDisposable("kosabi_stubgen")
    try {
      val session =
          buildStandaloneAnalysisAPISession(
              projectDisposable = projectDisposable,
              classLoader =
                  requireNotNull(StubsCodegenK2FirAnalysisHandlerExtension::class.java.classLoader),
          ) {
            buildKtModuleProviderByCompilerConfiguration(configuration)
          }

      val (module, files) = session.modulesWithFiles.entries.single()
      val ktFiles = files.filterIsInstance<KtFile>()
      StubsGenAPI( // @oss-enable
      // @oss-disable: StubsGenApiImpl(
              stubsDumpDir,
              stubsClassOutputDir,
              classPaths,
          )
          .generateStubs(ktFiles, configuration, session.project)
    } finally {
      Disposer.dispose(projectDisposable)
      throw RuntimeException("Terminating compilation. We're done with Stubgen.")
    }

    return true
  }
}
