/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

@file:Suppress(
    "PackageLocationMismatch",
    "DEPRECATION",
    "DEPRECATION_ERROR",
)
@file:OptIn(org.jetbrains.kotlin.cli.common.LegacyK2CliPipeline::class)

package com.facebook

import org.jetbrains.kotlin.cli.common.messages.MessageCollector
import org.jetbrains.kotlin.cli.jvm.compiler.VfsBasedProjectEnvironment
import org.jetbrains.kotlin.cli.jvm.compiler.createLibraryListForJvm
import org.jetbrains.kotlin.config.CompilerConfiguration
import org.jetbrains.kotlin.config.JVMConfigurationKeys
import org.jetbrains.kotlin.config.languageVersionSettings
import org.jetbrains.kotlin.diagnostics.DiagnosticReporterFactory
import org.jetbrains.kotlin.diagnostics.impl.BaseDiagnosticsCollector
import org.jetbrains.kotlin.fir.FirSession
import org.jetbrains.kotlin.fir.declarations.FirFile
import org.jetbrains.kotlin.fir.declarations.FirSimpleFunction
import org.jetbrains.kotlin.fir.extensions.FirExtensionRegistrar
import org.jetbrains.kotlin.fir.java.FirProjectSessionProvider
import org.jetbrains.kotlin.fir.pipeline.FirResult
import org.jetbrains.kotlin.fir.pipeline.ModuleCompilerAnalyzedOutput
import org.jetbrains.kotlin.fir.resolve.ScopeSession
import org.jetbrains.kotlin.fir.session.FirJvmIncrementalCompilationSymbolProviders
import org.jetbrains.kotlin.fir.session.FirSharableJavaComponents
import org.jetbrains.kotlin.fir.session.IncrementalCompilationContext
import org.jetbrains.kotlin.fir.session.createSymbolProviders
import org.jetbrains.kotlin.fir.session.environment.AbstractProjectFileSearchScope
import org.jetbrains.kotlin.fir.session.firCachesFactoryForCliMode
import org.jetbrains.kotlin.fir.visitors.FirDefaultVisitorVoid
import org.jetbrains.kotlin.name.Name
import org.jetbrains.kotlin.platform.jvm.JvmPlatforms
import org.jetbrains.kotlin.psi.KtFile

/** Frontend analysis output wrapper. Renamed to AllModulesFrontendOutput in Kotlin 2.3. */
typealias FrontendOutputCompat = FirResult

/** Per-module frontend output element. Renamed to SingleModuleFrontendOutput in Kotlin 2.3. */
typealias ModuleFrontendOutputCompat = ModuleCompilerAnalyzedOutput

/** Source function declaration. Renamed to FirNamedFunction in Kotlin 2.3. */
typealias FirNamedFunctionCompat = FirSimpleFunction

fun moduleFrontendOutputCompat(
    session: FirSession,
    scopeSession: ScopeSession,
    fir: List<FirFile>,
): ModuleFrontendOutputCompat = ModuleCompilerAnalyzedOutput(session, scopeSession, fir)

fun frontendOutputCompat(outputs: List<ModuleFrontendOutputCompat>): FrontendOutputCompat =
    FirResult(outputs)

/**
 * FIR visitor base that bridges the function visit method renamed in Kotlin 2.3
 * (visitSimpleFunction -> visitNamedFunction). Subclasses implement [visitNamedFunctionCompat].
 */
abstract class FirNamedFunctionVisitorCompat : FirDefaultVisitorVoid() {
  final override fun visitSimpleFunction(simpleFunction: FirSimpleFunction) {
    visitNamedFunctionCompat(simpleFunction)
  }

  abstract fun visitNamedFunctionCompat(function: FirNamedFunctionCompat)
}

/** createPendingReporter dropped its MessageCollector parameter in Kotlin 2.3. */
fun createPendingReporterCompat(messageCollector: MessageCollector): BaseDiagnosticsCollector =
    DiagnosticReporterFactory.createPendingReporter(messageCollector)

/** ModuleCompilerOutput.generationState.factory was flattened to .factory in Kotlin 2.3. */
val ModuleCompilerOutputCompat.classFileFactoryCompat
  get() = generationState.factory

/** RETAIN_OUTPUT_IN_MEMORY is required to read bytecode from memory before Kotlin 2.3. */
fun CompilerConfiguration.retainOutputInMemoryCompat() {
  put(JVMConfigurationKeys.RETAIN_OUTPUT_IN_MEMORY, true)
}

/**
 * Prepares the FIR JVM session(s) for the given source files. Kotlin 2.3 replaced the manual
 * session-construction pipeline with JvmFrontendPipelinePhase.prepareJvmSessions.
 */
fun prepareJvmSessionsCompat(
    ktFiles: List<KtFile>,
    rootModuleName: String,
    friendPaths: List<String>,
    librariesScope: AbstractProjectFileSearchScope,
    configuration: CompilerConfiguration,
    projectEnvironment: VfsBasedProjectEnvironment,
    providerAndScopeForIncrementalCompilation: IncrementalCompilationContext?,
): List<Pair<FirSession, List<KtFile>>> {
  val libraryList = createLibraryListForJvm(rootModuleName, configuration, friendPaths)
  val moduleName = Name.special("<$rootModuleName>")

  val extensionRegistrars = FirExtensionRegistrar.getInstances(projectEnvironment.project)
  val javaSourcesScope = projectEnvironment.getSearchScopeForProjectJavaSources()
  val predefinedJavaComponents = FirSharableJavaComponents(firCachesFactoryForCliMode)

  val sessionProvider = FirProjectSessionProvider()

  createLibrarySessionCompat(
      moduleName,
      sessionProvider,
      libraryList.moduleDataProvider,
      projectEnvironment,
      extensionRegistrars,
      librariesScope,
      projectEnvironment.getPackagePartProvider(librariesScope),
      configuration.languageVersionSettings,
      predefinedJavaComponents,
  )

  val platformModuleData =
      createSourceModuleData(
          moduleName,
          libraryList.regularDependencies,
          libraryList.dependsOnDependencies,
          libraryList.friendDependenciesCompat,
          JvmPlatforms.unspecifiedJvmPlatform,
      )

  var incrementalSymbolProviders: FirJvmIncrementalCompilationSymbolProviders? = null
  var incrementalSymbolProvidersInitialized = false

  val session =
      createSourceSessionCompat(
          platformModuleData,
          sessionProvider,
          javaSourcesScope,
          projectEnvironment,
          createIncrementalCompilationSymbolProviders = { firSession ->
            if (incrementalSymbolProvidersInitialized) incrementalSymbolProviders
            else {
              incrementalSymbolProvidersInitialized = true
              providerAndScopeForIncrementalCompilation
                  ?.createSymbolProviders(firSession, platformModuleData, projectEnvironment)
                  ?.also { incrementalSymbolProviders = it }
            }
          },
          extensionRegistrars,
          configuration,
          predefinedJavaComponents = predefinedJavaComponents,
          needRegisterJavaElementFinder = true,
          init = {},
      )

  return listOf(session to ktFiles)
}
