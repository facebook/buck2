/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.kotlin.compilerplugins.kosabi.stubsgen.plugin.stubsgen

import com.facebook.kotlin.compilerplugins.kosabi.common.Logger
import com.facebook.kotlin.compilerplugins.kosabi.common.stub.model.KStub
import com.facebook.kotlin.compilerplugins.kosabi.common.stub.render.RenderedKStub
import com.facebook.kotlin.compilerplugins.kosabi.common.stub.render.render
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.config.knownKSPGeneratedTypes
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.generators.AnnotationStubsGenerator
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.generators.ClassLevelFunctionStubsGenerator
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.generators.CtorStubsGenerator
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.generators.FullQualifiedClassStubsGenerator
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.generators.GenerationContext
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.generators.GenericStubsGenerator
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.generators.InnerClassStubsGenerator
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.generators.InterfaceStubsGenerator
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.generators.PrimitiveStubsGenerator
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.generators.SamePackageClassStubsGenerator
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.generators.StubsGenerator
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.generators.apemulators.ApEmulatorStubsGenerator
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.generators.custom.CustomStubsGenerator
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.plugin.model.generateFakeKtFile
import java.io.File
import java.nio.file.Files
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
    private val additionalSourcesGenerators: List<StubsGenerator> =
        listOf(ApEmulatorStubsGenerator()),
    private val stubsDumpDir: File? = null,
    private val classPaths: List<File>
) : CollectAdditionalSourcesExtension {
  // Stubs generation should happen exactly once.
  // `collectAdditionalSourcesAndUpdateConfiguration` will happen again with the files previously
  // created by `collectAdditionalSourcesAndUpdateConfiguration`
  var alreadyCreated = false

  override fun collectAdditionalSourcesAndUpdateConfiguration(
      knownSources: Collection<KtFile>,
      configuration: CompilerConfiguration,
      project: Project
  ): Collection<KtFile> {
    if (alreadyCreated) {
      return emptyList()
    }

    alreadyCreated = true

    // Why we have two rounds of `generateStubs` here?
    //
    // 1. [additionalSourcesGenerators] creates a set of additional sources ([KStub]s that we treat
    // as real files)
    // An additional source file is usually a complicated stub that is included in ABI.
    //
    // 2. If we run [additionalSourcesGenerators] as a part of [CustomStubsGenerator]
    // we will need to run all the stubs that generate stub classes structure (primitive, ctor, ...)
    // for types that we introduced during [additionalSourcesGenerators]
    //   2a. or alternatively create all these stubs (but it's a lot of work)
    //
    // 3. So we call [additionalSourcesGenerators] with it's own [generationContext]
    //   3a. Than dump context creating additional source files (similar to what AP usually does)
    //   3b. Than run the second round  with extended [knownSources]
    Logger.log("[Source files] ${knownSources.joinToString(", ") { it.name }}")
    val additionalSrcStubsContext = GenerationContext(knownSources, classPaths, lightweight = true)
    val additionalSrcStubs = generateStubs(additionalSourcesGenerators, additionalSrcStubsContext)
    val additionalRenderedSrcStubs: List<RenderedKStub> =
        additionalSrcStubs.map { RenderedKStub(it, it.render()) }
    val additionalSrcs = additionalRenderedSrcStubs.map { generateFakeKtFile(project, it) }

    val stubsGenerators: List<StubsGenerator> =
        listOf(
            PrimitiveStubsGenerator(),
            InnerClassStubsGenerator(),
            FullQualifiedClassStubsGenerator(),
            SamePackageClassStubsGenerator(),
            CtorStubsGenerator(),
            InterfaceStubsGenerator(),
            GenericStubsGenerator(),
            AnnotationStubsGenerator(),
            ClassLevelFunctionStubsGenerator(),
            CustomStubsGenerator(),
        )
    val context =
        GenerationContext(
            additionalSrcs + knownSources, classPaths, knownKSPGeneratedTypes, lightweight = false)
    val stubs = generateStubs(stubsGenerators, context)
    val renderedStubs: List<RenderedKStub> = stubs.map { RenderedKStub(it, it.render()) }
    val stubFiles = renderedStubs.map { generateFakeKtFile(project, it) }

    if (stubsDumpDir != null) {
      val stubDir = stubsDumpDir
      check(stubDir.exists() || stubDir.mkdirs()) {
        "Could not generate package directory: $stubDir"
      }

      if (configuration.getBoolean(stubsGenStandaloneModeKey)) {
        (additionalSrcs + stubFiles).forEach { file ->
          File(stubsDumpDir, file.name).writeText(file.text)
        }
      } else {
        writeStubs(stubDir, renderedStubs + additionalRenderedSrcStubs)
      }
    }

    if (configuration.getBoolean(stubsGenStandaloneModeKey)) {
      throw RuntimeException("Terminating compilation. We're done with Stubgen.")
    }

    return additionalSrcs + stubFiles
  }

  fun writeStubs(dir: File, stubs: Collection<RenderedKStub>) {
    stubs.forEach { renderedStub ->
      val relativeDir = renderedStub.stub.pkg?.replace(".", "/")
      val targetDir = if (relativeDir.isNullOrEmpty()) dir else dir.resolve(relativeDir)
      Files.createDirectories(targetDir.toPath())
      val filename = "${renderedStub.stub.name}.kt"
      File(targetDir, filename).writeText(renderedStub.render)
    }
  }

  private fun generateStubs(
      generators: List<StubsGenerator>,
      context: GenerationContext
  ): Collection<KStub> {
    generators.forEach { generator ->
      Logger.log("[${generator.javaClass.simpleName}]")
      generator.generateStubs(context)
    }
    return context.stubsContainer.all()
  }
}
