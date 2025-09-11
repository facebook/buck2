/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.kotlin.compilerplugins.kosabi.stubsgen.plugin.stubsgen

import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.generators.StubsGenAPI // @oss-enable
// @oss-disable: import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.generators.meta_only.StubsGenApiImpl
import java.io.File
import org.jetbrains.kotlin.com.intellij.openapi.project.Project
import org.jetbrains.kotlin.config.CompilerConfiguration
import org.jetbrains.kotlin.extensions.CollectAdditionalSourcesExtension
import org.jetbrains.kotlin.psi.KtFile

/**
 * [StubsAdditionalSourcesExtension] applies code generators to all `knownSources` files, collects
 * generated stubs, and then adds newly created stubs for further analysis.
 *
 * This implementation does not have [Module] in Extension parameters. This Extension executes
 * before all Analysis Extensions, so it's NOT POSSIBLE to get data from Analysis extensions here.
 *
 * [StubsAdditionalSourcesExtension] fakes KtFiles without having a properly created [VirtualFile]
 * anywhere in [VirtualFileSystem].
 *
 * Every [VirtualFile] fakes its path with dummy.kt
 *
 * TODO: Possible better implementation
 * 1. Create `AnalysisResult.RetryWithAdditionalRoots` in Kotlin Compiler. Class should take a
 *    `List<KtFile>` as `additionalKotlinFiles` argument, skip File -> KtFile mapping stages and
 *    pass LightVirtualFile directly to the next analysis stage.
 * 2. Contribute back
 */
class StubsAdditionalSourcesExtension(
    private val stubsDumpDir: File? = null,
    private val stubsClassOutputDir: File? = null,
    private val classPaths: List<File>,
) : CollectAdditionalSourcesExtension {
  // Stubs generation should happen exactly once.
  // `collectAdditionalSourcesAndUpdateConfiguration` will happen again with the files previously
  // created by `collectAdditionalSourcesAndUpdateConfiguration`
  var alreadyCreated = false

  override fun collectAdditionalSourcesAndUpdateConfiguration(
      knownSources: Collection<KtFile>,
      configuration: CompilerConfiguration,
      project: Project,
  ): Collection<KtFile> {
    if (alreadyCreated) {
      return emptyList()
    }

    alreadyCreated = true

    val stubgenAPI =
        StubsGenAPI( // @oss-enable
        // @oss-disable: StubsGenApiImpl(
            stubsDumpDir,
            stubsClassOutputDir,
            classPaths,
        )

    val stubs = stubgenAPI.generateStubs(knownSources, configuration, project)

    throw RuntimeException("Terminating compilation. We're done with Stubgen.")

    return stubs
  }
}
