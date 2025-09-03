/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook

import java.io.File
import org.jetbrains.kotlin.backend.common.extensions.IrGenerationExtension
import org.jetbrains.kotlin.backend.common.extensions.IrPluginContext
import org.jetbrains.kotlin.cli.common.SessionWithSources
import org.jetbrains.kotlin.cli.common.config.KotlinSourceRoot
import org.jetbrains.kotlin.cli.common.config.kotlinSourceRoots
import org.jetbrains.kotlin.cli.common.messages.CompilerMessageLocation
import org.jetbrains.kotlin.cli.common.messages.CompilerMessageSeverity
import org.jetbrains.kotlin.cli.common.messages.MessageCollector
import org.jetbrains.kotlin.cli.jvm.compiler.EnvironmentConfigFiles
import org.jetbrains.kotlin.cli.jvm.compiler.FirKotlinToJvmBytecodeCompiler
import org.jetbrains.kotlin.cli.jvm.compiler.KotlinCoreEnvironment
import org.jetbrains.kotlin.cli.jvm.compiler.VfsBasedProjectEnvironment
import org.jetbrains.kotlin.cli.jvm.compiler.createContextForIncrementalCompilation
import org.jetbrains.kotlin.cli.jvm.compiler.createLibraryListForJvm
import org.jetbrains.kotlin.cli.jvm.compiler.pipeline.ModuleCompilerEnvironment
import org.jetbrains.kotlin.cli.jvm.compiler.pipeline.ModuleCompilerOutput
import org.jetbrains.kotlin.cli.jvm.compiler.pipeline.convertAnalyzedFirToIr
import org.jetbrains.kotlin.cli.jvm.compiler.pipeline.createProjectEnvironment
import org.jetbrains.kotlin.cli.jvm.compiler.pipeline.generateCodeFromIr
import org.jetbrains.kotlin.cli.jvm.compiler.report
import org.jetbrains.kotlin.com.intellij.openapi.diagnostic.Logger
import org.jetbrains.kotlin.com.intellij.openapi.project.Project
import org.jetbrains.kotlin.com.intellij.openapi.util.Disposer
import org.jetbrains.kotlin.com.intellij.openapi.vfs.StandardFileSystems
import org.jetbrains.kotlin.com.intellij.openapi.vfs.VirtualFile
import org.jetbrains.kotlin.com.intellij.openapi.vfs.VirtualFileManager
import org.jetbrains.kotlin.com.intellij.psi.PsiManager
import org.jetbrains.kotlin.config.CommonConfigurationKeys
import org.jetbrains.kotlin.config.CompilerConfiguration
import org.jetbrains.kotlin.config.JVMConfigurationKeys
import org.jetbrains.kotlin.config.JvmTarget
import org.jetbrains.kotlin.config.languageVersionSettings
import org.jetbrains.kotlin.config.messageCollector
import org.jetbrains.kotlin.diagnostics.DiagnosticReporterFactory
import org.jetbrains.kotlin.diagnostics.impl.BaseDiagnosticsCollector
import org.jetbrains.kotlin.extensions.CompilerConfigurationExtension
import org.jetbrains.kotlin.extensions.PreprocessedFileCreator
import org.jetbrains.kotlin.fir.DependencyListForCliModule
import org.jetbrains.kotlin.fir.FirModuleData
import org.jetbrains.kotlin.fir.FirModuleDataImpl
import org.jetbrains.kotlin.fir.FirSession
import org.jetbrains.kotlin.fir.extensions.FirAnalysisHandlerExtension
import org.jetbrains.kotlin.fir.extensions.FirExtensionRegistrar
import org.jetbrains.kotlin.fir.java.FirProjectSessionProvider
import org.jetbrains.kotlin.fir.pipeline.FirResult
import org.jetbrains.kotlin.fir.pipeline.buildResolveAndCheckFirFromKtFiles
import org.jetbrains.kotlin.fir.session.FirJvmIncrementalCompilationSymbolProviders
import org.jetbrains.kotlin.fir.session.FirJvmSessionFactory
import org.jetbrains.kotlin.fir.session.FirSessionConfigurator
import org.jetbrains.kotlin.fir.session.FirSharableJavaComponents
import org.jetbrains.kotlin.fir.session.IncrementalCompilationContext
import org.jetbrains.kotlin.fir.session.createSymbolProviders
import org.jetbrains.kotlin.fir.session.environment.AbstractProjectFileSearchScope
import org.jetbrains.kotlin.fir.session.firCachesFactoryForCliMode
import org.jetbrains.kotlin.idea.KotlinFileType
import org.jetbrains.kotlin.ir.IrBuiltIns
import org.jetbrains.kotlin.ir.IrStatement
import org.jetbrains.kotlin.ir.declarations.IrAnonymousInitializer
import org.jetbrains.kotlin.ir.declarations.IrClass
import org.jetbrains.kotlin.ir.declarations.IrConstructor
import org.jetbrains.kotlin.ir.declarations.IrDeclarationBase
import org.jetbrains.kotlin.ir.declarations.IrDeclarationContainer
import org.jetbrains.kotlin.ir.declarations.IrDeclarationWithVisibility
import org.jetbrains.kotlin.ir.declarations.IrFactory
import org.jetbrains.kotlin.ir.declarations.IrFile
import org.jetbrains.kotlin.ir.declarations.IrModuleFragment
import org.jetbrains.kotlin.ir.declarations.IrProperty
import org.jetbrains.kotlin.ir.declarations.IrSimpleFunction
import org.jetbrains.kotlin.ir.expressions.IrBody
import org.jetbrains.kotlin.ir.expressions.IrExpression
import org.jetbrains.kotlin.ir.expressions.IrExpressionBody
import org.jetbrains.kotlin.ir.expressions.IrReturn
import org.jetbrains.kotlin.ir.expressions.IrStatementOrigin
import org.jetbrains.kotlin.ir.expressions.impl.IrConstImpl
import org.jetbrains.kotlin.ir.expressions.impl.IrGetValueImpl
import org.jetbrains.kotlin.ir.expressions.impl.IrReturnImpl
import org.jetbrains.kotlin.ir.symbols.IrReturnTargetSymbol
import org.jetbrains.kotlin.ir.symbols.UnsafeDuringIrConstructionAPI
import org.jetbrains.kotlin.ir.types.IrSimpleType
import org.jetbrains.kotlin.ir.types.IrType
import org.jetbrains.kotlin.ir.types.makeNullable
import org.jetbrains.kotlin.ir.util.statements
import org.jetbrains.kotlin.ir.visitors.IrElementTransformer
import org.jetbrains.kotlin.modules.Module
import org.jetbrains.kotlin.modules.TargetId
import org.jetbrains.kotlin.name.Name
import org.jetbrains.kotlin.platform.TargetPlatform
import org.jetbrains.kotlin.platform.jvm.JvmPlatforms
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.kotlin.resolve.multiplatform.hmppModuleName
import org.jetbrains.kotlin.resolve.multiplatform.isCommonSource

@SuppressWarnings("PackageLocationMismatch")
class K2JvmAbiFirAnalysisHandlerExtension(private val outputPath: String) :
    FirAnalysisHandlerExtension() {

  override fun isApplicable(configuration: CompilerConfiguration): Boolean {
    return true
  }

  override fun doAnalysis(project: Project, configuration: CompilerConfiguration): Boolean {
    val updatedConfiguration =
        configuration.copy().apply {
          put(JVMConfigurationKeys.SKIP_BODIES, true)
          put(JVMConfigurationKeys.RETAIN_OUTPUT_IN_MEMORY, true)
          put(JVMConfigurationKeys.VALIDATE_BYTECODE, true)
        }
    // the disposable is responsible to dispose the project after the analysis is done
    val disposable = Disposer.newDisposable("K2KosabiSession.project")
    try {
      val projectEnvironment =
          createProjectEnvironment(
              updatedConfiguration,
              disposable,
              EnvironmentConfigFiles.JVM_CONFIG_FILES,
              configuration.messageCollector,
          )
      if (updatedConfiguration.messageCollector.hasErrors()) {
        return false
      }
      val messageCollector =
          updatedConfiguration.getNotNull(CommonConfigurationKeys.MESSAGE_COLLECTOR_KEY)
      val module =
          updatedConfiguration[JVMConfigurationKeys.MODULES]?.single()
              ?: error(
                  "Single module expected: ${updatedConfiguration[JVMConfigurationKeys.MODULES]}"
              )

      val sourceFiles =
          createSourceFilesFromSourceRoots(
              updatedConfiguration,
              projectEnvironment.project,
              configuration.kotlinSourceRoots,
          )
      IrGenerationExtension.registerExtension(
          projectEnvironment.project,
          NonAbiDeclarationsStrippingIrExtension(),
      )
      val analysisResults =
          runFrontendForKosabi(
              projectEnvironment,
              updatedConfiguration,
              messageCollector,
              sourceFiles,
              module,
          )
      runBackendForKosabi(
          messageCollector,
          projectEnvironment,
          updatedConfiguration,
          module,
          analysisResults,
      )
    } finally {
      Disposer.dispose(disposable)
    }
    return true
  }

  // A backend entry point for Kosabi.
  private fun runBackendForKosabi(
      messageCollector: MessageCollector,
      projectEnvironment: VfsBasedProjectEnvironment,
      configuration: CompilerConfiguration,
      module: Module,
      analysisResults: FirResult,
  ): ModuleCompilerOutput {
    // Ignore all FE errors
    val cleanDiagnosticReporter =
        FirKotlinToJvmBytecodeCompiler.createPendingReporter(messageCollector)
    val compilerEnvironment = ModuleCompilerEnvironment(projectEnvironment, cleanDiagnosticReporter)
    val irInput =
        convertAnalyzedFirToIr(
            configuration,
            TargetId(module),
            analysisResults,
            compilerEnvironment,
        )

    return generateCodeFromIr(irInput, compilerEnvironment)
  }

  private class NonAbiDeclarationsStrippingIrExtension : IrGenerationExtension {
    override fun generate(moduleFragment: IrModuleFragment, pluginContext: IrPluginContext) {
      moduleFragment.transform(
          NonAbiDeclarationsStrippingIrVisitor(pluginContext.irFactory, pluginContext.irBuiltIns),
          null,
      )
    }
  }

  @OptIn(UnsafeDuringIrConstructionAPI::class)
  private class NonAbiDeclarationsStrippingIrVisitor(
      private val irFactory: IrFactory,
      private val irBuiltins: IrBuiltIns,
  ) : IrElementTransformer<Nothing?> {

    override fun visitFile(declaration: IrFile, data: Nothing?): IrFile {
      declaration.removeNonPublicApi()
      return super.visitFile(declaration, data)
    }

    override fun visitDeclaration(declaration: IrDeclarationBase, data: Nothing?): IrStatement {
      if (declaration is IrDeclarationContainer) {
        declaration.removeNonPublicApi()
      }
      return super.visitDeclaration(declaration, data)
    }

    override fun visitClass(declaration: IrClass, data: Nothing?): IrStatement {
      return super.visitClass(declaration, data)
    }

    // we shouldn't generate default values for constants becuase the values are getting inlined
    // TODO: fix it: T235115614
    private fun generateDefaultValue(type: IrType): IrExpression? {
      val constructedType = type as? IrSimpleType ?: return null

      // For primitive types, create a default constant value
      // The code handles properties like val DEFAULT_SHADOW_COLOR: Int = Color.argb(128, 0, 0, 0)
      // If we don't set a default value, compiler crashes because it expects a ConstExpression
      var defaultValue: IrExpression? =
          when {
            constructedType == irBuiltins.intType -> IrConstImpl.int(-1, -1, irBuiltins.intType, 0)
            constructedType == irBuiltins.booleanType ->
                IrConstImpl.boolean(-1, -1, irBuiltins.booleanType, false)
            constructedType == irBuiltins.stringType ->
                IrConstImpl.string(-1, -1, irBuiltins.stringType, "")
            constructedType == irBuiltins.doubleType ->
                IrConstImpl.double(-1, -1, irBuiltins.doubleType, 0.0)
            constructedType == irBuiltins.floatType ->
                IrConstImpl.float(-1, -1, irBuiltins.floatType, 0.0f)
            constructedType == irBuiltins.longType ->
                IrConstImpl.long(-1, -1, irBuiltins.longType, 0L)
            constructedType == irBuiltins.charType ->
                IrConstImpl.char(-1, -1, irBuiltins.charType, '\u0000')
            constructedType == irBuiltins.byteType ->
                IrConstImpl.byte(-1, -1, irBuiltins.byteType, 0)
            constructedType == irBuiltins.shortType ->
                IrConstImpl.short(-1, -1, irBuiltins.shortType, 0)
            else ->
                IrConstImpl.constNull(
                    -1,
                    -1,
                    constructedType.makeNullable(),
                ) // If we can't handle this type, return null
          }

      return defaultValue
    }

    private fun IrDeclarationContainer.removeNonPublicApi() {
      this.declarations.removeAll { declaration ->
        (!declaration.origin.isSynthetic &&
            declaration !is IrConstructor &&
            (declaration as? IrDeclarationWithVisibility)?.visibility?.isPublicAPI == false) &&
            (declaration as? IrClass)?.isCompanion == true
      }
    }

    private fun generateDefaultReturnStatement(
        type: IrType,
        symbol: IrReturnTargetSymbol,
    ): IrReturn? {
      val defaultValue = generateDefaultValue(type) ?: return null
      return IrReturnImpl(-1, -1, irBuiltins.nothingType, symbol, defaultValue)
    }

    private fun generateDefaultBody(type: IrType, symbol: IrReturnTargetSymbol): IrBody? {
      val returnStatement = generateDefaultReturnStatement(type, symbol) ?: return null
      return irFactory.createBlockBody(-1, -1).apply { this.statements.add(returnStatement) }
    }

    private fun generateDefaultExpressionBody(type: IrType): IrExpressionBody? {
      val defaultValue = generateDefaultValue(type) ?: return null
      return irFactory.createExpressionBody(-1, -1, defaultValue)
    }

    override fun visitSimpleFunction(declaration: IrSimpleFunction, data: Nothing?): IrStatement {
      if (!declaration.origin.isSynthetic) {
        if (declaration.parent is IrProperty) {
          // for properties we need to generate a default body
          // otherwise we get a crash in the compiler
          // handles properties likeval DEFAULT_SHADOW_COLOR: Int = Color.argb(128, 0, 0, 0)
          // we shouldn't generate default values for constants becuase the values are getting
          // inlined
          // TODO: fix it: T235115614
          val body = generateDefaultBody(declaration.returnType, declaration.symbol)
          declaration.body = body ?: irFactory.createBlockBody(-1, -1)
        } else {
          declaration.body = irFactory.createBlockBody(-1, -1)
        }
        val parametersWithDefaultValues =
            declaration.valueParameters.filter { it.defaultValue != null }
        for (parameter in parametersWithDefaultValues) {
          // if we can - we resolve the default value to a constant
          // handles default values in functions like fun foo(x: Int = Something.SomeValue)
          generateDefaultExpressionBody(parameter.type)?.let { parameter.defaultValue = it }
        }
      }
      return super.visitSimpleFunction(declaration, data)
    }

    override fun visitProperty(declaration: IrProperty, data: Nothing?): IrStatement {
      if (declaration.parent is IrClass || declaration.parent is IrFile) {
        if (declaration.parent !is IrDeclarationContainer || !declaration.origin.isSynthetic) {

          // the compiler generates backing fields for properties
          // initializers for the backing fields may contain missing types
          val backingField = declaration.backingField
          if (
              backingField != null &&
                  (backingField.initializer?.expression as? IrGetValueImpl)?.origin !=
                      IrStatementOrigin.INITIALIZE_PROPERTY_FROM_PARAMETER &&
                  backingField.initializer != null
          ) {
            backingField.initializer = null
          }
        }
      }
      return super.visitProperty(declaration, data)
    }

    override fun visitAnonymousInitializer(
        declaration: IrAnonymousInitializer,
        data: Nothing?,
    ): IrStatement {
      // we also need to strip bodies from init {} blocks
      declaration.body = irFactory.createBlockBody(-1, -1)
      return super.visitAnonymousInitializer(declaration, data)
    }
  }

  // get all the source files from the source roots and allows the caller to process them
  fun List<KotlinSourceRoot>.forAllFiles(
      configuration: CompilerConfiguration,
      project: Project,
      reportLocation: CompilerMessageLocation? = null,
      body: (VirtualFile, Boolean, moduleName: String?) -> Unit,
  ) {
    val localFileSystem =
        VirtualFileManager.getInstance().getFileSystem(StandardFileSystems.FILE_PROTOCOL)

    val processedFiles = mutableSetOf<VirtualFile>()

    val virtualFileCreator = PreprocessedFileCreator(project)

    var pluginsConfigured = false
    fun ensurePluginsConfigured() {
      if (!pluginsConfigured) {
        for (extension in CompilerConfigurationExtension.getInstances(project)) {
          extension.updateFileRegistry()
        }
        pluginsConfigured = true
      }
    }

    for ((sourceRootPath, isCommon, hmppModuleName) in this) {
      val sourceRoot = File(sourceRootPath)
      val vFile = localFileSystem.findFileByPath(sourceRoot.normalize().path)
      if (vFile == null) {
        val message = "Source file or directory not found: $sourceRootPath"

        val buildFilePath = configuration.get(JVMConfigurationKeys.MODULE_XML_FILE)
        if (buildFilePath != null && Logger.isInitialized()) {
          Logger.getInstance(KotlinCoreEnvironment::class.java)
              .warn(
                  "$message\n\nbuild file path: $buildFilePath\ncontent:\n${buildFilePath.readText()}"
              )
        }

        configuration.report(CompilerMessageSeverity.ERROR, message, reportLocation)
        continue
      }

      if (!vFile.isDirectory && vFile.extension != KotlinFileType.EXTENSION) {
        ensurePluginsConfigured()
        if (vFile.fileType != KotlinFileType.INSTANCE) {
          configuration.report(
              CompilerMessageSeverity.ERROR,
              "Source entry is not a Kotlin file: $sourceRootPath",
              reportLocation,
          )
          continue
        }
      }

      for (file in sourceRoot.walkTopDown()) {
        if (!file.isFile) continue

        val virtualFile: VirtualFile? =
            localFileSystem.findFileByPath(file.absoluteFile.normalize().path)?.let {
              virtualFileCreator.create(it)
            }
        if (virtualFile != null) {
          val hasNotBeenProcessedBefore = processedFiles.add(virtualFile)
          if (!hasNotBeenProcessedBefore) continue
          if (virtualFile.extension != KotlinFileType.EXTENSION) {
            ensurePluginsConfigured()
          }
          if (
              virtualFile.extension == KotlinFileType.EXTENSION ||
                  virtualFile.fileType == KotlinFileType.INSTANCE
          ) {
            body(virtualFile, isCommon, hmppModuleName)
          }
        }
      }
    }
  }

  // get all the source files from the source roots and matches them with corresponding PSI
  fun createSourceFilesFromSourceRoots(
      configuration: CompilerConfiguration,
      project: Project,
      sourceRoots: List<KotlinSourceRoot>,
      reportLocation: CompilerMessageLocation? = null,
  ): MutableList<KtFile> {
    val psiManager = PsiManager.getInstance(project)
    val result = mutableListOf<KtFile>()
    sourceRoots.forAllFiles(configuration, project, reportLocation) {
        virtualFile,
        isCommon,
        moduleName ->
      psiManager.findFile(virtualFile)?.let {
        if (it is KtFile) {
          it.isCommonSource = isCommon
          if (moduleName != null) {
            it.hmppModuleName = moduleName
          }
          result.add(it)
        }
      }
    }
    return result
  }

  // A frontend entry point for Kosabi.
  // Generates FIR for all the sources in the module.
  fun runFrontendForKosabi(
      environment: VfsBasedProjectEnvironment,
      configuration: CompilerConfiguration,
      messageCollector: MessageCollector,
      sources: List<KtFile>,
      module: Module,
  ): FirResult {
    val diagnosticsReporter = DiagnosticReporterFactory.createPendingReporter(messageCollector)
    return compileSourceFilesToAnalyzedFirViaPsi(
        sources,
        diagnosticsReporter,
        module.getModuleName(),
        module.getFriendPaths(),
        true,
        environment,
        configuration,
    )!!
  }

  private fun compileSourceFilesToAnalyzedFirViaPsi(
      ktFiles: List<KtFile>,
      diagnosticsReporter: BaseDiagnosticsCollector,
      rootModuleName: String,
      friendPaths: List<String>,
      ignoreErrors: Boolean = false,
      projectEnvironment: VfsBasedProjectEnvironment,
      configuration: CompilerConfiguration,
  ): FirResult? {

    val sourceScope =
        projectEnvironment.getSearchScopeByPsiFiles(ktFiles) +
            projectEnvironment.getSearchScopeForProjectJavaSources()

    var librariesScope = projectEnvironment.getSearchScopeForProjectLibraries()

    val providerAndScopeForIncrementalCompilation =
        createContextForIncrementalCompilation(projectEnvironment, configuration, sourceScope)

    providerAndScopeForIncrementalCompilation?.precompiledBinariesFileScope?.let {
      librariesScope -= it
    }
    val sessionsWithSources =
        prepareJvmSessions(
            ktFiles,
            rootModuleName,
            friendPaths,
            librariesScope,
            isCommonSource = { false },
            isScript = { false },
            fileBelongsToModule = { file: KtFile, moduleName: String ->
              file.hmppModuleName == moduleName
            },
            createProviderAndScopeForIncrementalCompilation = {
              providerAndScopeForIncrementalCompilation
            },
            projectEnvironment,
            configuration,
        )

    val outputs =
        sessionsWithSources.map { (session, sources) ->
          buildResolveAndCheckFirFromKtFiles(session, sources, diagnosticsReporter)
        }

    return FirResult(outputs)
  }

  private fun <F> prepareJvmSessions(
      files: List<F>,
      rootModuleNameAsString: String,
      friendPaths: List<String>,
      librariesScope: AbstractProjectFileSearchScope,
      isCommonSource: (F) -> Boolean,
      isScript: (F) -> Boolean,
      fileBelongsToModule: (F, String) -> Boolean,
      createProviderAndScopeForIncrementalCompilation: (List<F>) -> IncrementalCompilationContext?,
      projectEnvironment: VfsBasedProjectEnvironment,
      configuration: CompilerConfiguration,
  ): List<SessionWithSources<F>> {
    val libraryList = createLibraryListForJvm(rootModuleNameAsString, configuration, friendPaths)
    val rootModuleName = Name.special("<$rootModuleNameAsString>")
    return prepareJvmSessions(
        files,
        rootModuleName,
        librariesScope,
        libraryList,
        isCommonSource,
        isScript,
        fileBelongsToModule,
        createProviderAndScopeForIncrementalCompilation,
        projectEnvironment,
        configuration,
    )
  }

  private fun <F> prepareJvmSessions(
      files: List<F>,
      rootModuleName: Name,
      librariesScope: AbstractProjectFileSearchScope,
      libraryList: DependencyListForCliModule,
      isCommonSource: (F) -> Boolean,
      isScript: (F) -> Boolean,
      fileBelongsToModule: (F, String) -> Boolean,
      createProviderAndScopeForIncrementalCompilation: (List<F>) -> IncrementalCompilationContext?,
      projectEnvironment: VfsBasedProjectEnvironment,
      configuration: CompilerConfiguration,
  ): List<SessionWithSources<F>> {
    val extensionRegistrars = FirExtensionRegistrar.getInstances(projectEnvironment.project)
    val javaSourcesScope = projectEnvironment.getSearchScopeForProjectJavaSources()
    val predefinedJavaComponents = FirSharableJavaComponents(firCachesFactoryForCliMode)

    var firJvmIncrementalCompilationSymbolProviders: FirJvmIncrementalCompilationSymbolProviders? =
        null
    var firJvmIncrementalCompilationSymbolProvidersIsInitialized = false

    return prepareSessions(
        files,
        configuration,
        rootModuleName,
        JvmPlatforms.unspecifiedJvmPlatform,
        metadataCompilationMode = false,
        libraryList,
        isCommonSource,
        isScript,
        fileBelongsToModule,
        createLibrarySession = { sessionProvider ->
          FirJvmSessionFactory.createLibrarySession(
              rootModuleName,
              sessionProvider,
              libraryList.moduleDataProvider,
              projectEnvironment,
              extensionRegistrars,
              librariesScope,
              projectEnvironment.getPackagePartProvider(librariesScope),
              configuration.languageVersionSettings,
              predefinedJavaComponents,
          )
        },
    ) { moduleFiles, moduleData, sessionProvider, sessionConfigurator ->
      FirJvmSessionFactory.createModuleBasedSession(
          moduleData,
          sessionProvider,
          javaSourcesScope,
          projectEnvironment,
          createIncrementalCompilationSymbolProviders = { session ->
            // Temporary solution for KT-61942 - we need to share the provider built on top of
            // previously compiled files,
            // because we do not distinguish classes generated from common and platform sources, so
            // may end up with the
            // same type loaded from both. And if providers are not shared, the types will not match
            // on the actualizing.
            // The proper solution would be to build IC providers only on class files generated for
            // the currently compiled module.
            // But to solve it we need to have a mapping from module to its class files.
            // TODO: reimplement with splitted providers after fixing KT-62686
            if (firJvmIncrementalCompilationSymbolProvidersIsInitialized)
                firJvmIncrementalCompilationSymbolProviders
            else {
              firJvmIncrementalCompilationSymbolProvidersIsInitialized = true
              createProviderAndScopeForIncrementalCompilation(moduleFiles)
                  ?.createSymbolProviders(session, moduleData, projectEnvironment)
                  ?.also { firJvmIncrementalCompilationSymbolProviders = it }
            }
          },
          extensionRegistrars,
          configuration.languageVersionSettings,
          configuration.get(JVMConfigurationKeys.JVM_TARGET, JvmTarget.DEFAULT),
          configuration.get(CommonConfigurationKeys.LOOKUP_TRACKER),
          configuration.get(CommonConfigurationKeys.ENUM_WHEN_TRACKER),
          configuration.get(CommonConfigurationKeys.IMPORT_TRACKER),
          predefinedJavaComponents = predefinedJavaComponents,
          needRegisterJavaElementFinder = true,
          sessionConfigurator,
      )
    }
  }

  private fun <F> prepareSessions(
      files: List<F>,
      configuration: CompilerConfiguration,
      rootModuleName: Name,
      targetPlatform: TargetPlatform,
      metadataCompilationMode: Boolean,
      libraryList: DependencyListForCliModule,
      isCommonSource: (F) -> Boolean,
      isScript: (F) -> Boolean,
      fileBelongsToModule: (F, String) -> Boolean,
      createLibrarySession: (FirProjectSessionProvider) -> FirSession,
      createSourceSession:
          (
              List<F>,
              FirModuleData,
              FirProjectSessionProvider,
              FirSessionConfigurator.() -> Unit,
          ) -> FirSession,
  ): List<SessionWithSources<F>> {
    val (_, nonScriptFiles) = files.partition(isScript)

    val sessionProvider = FirProjectSessionProvider()

    createLibrarySession(sessionProvider)

    val sessionConfigurator: FirSessionConfigurator.() -> Unit = {}

    val nonScriptSessions =
        listOf(
            createSingleSession(
                nonScriptFiles,
                rootModuleName,
                libraryList,
                targetPlatform,
                sessionProvider,
                sessionConfigurator,
                createSourceSession,
            )
        )
    return nonScriptSessions
  }

  private fun <F> createSingleSession(
      files: List<F>,
      rootModuleName: Name,
      libraryList: DependencyListForCliModule,
      targetPlatform: TargetPlatform,
      sessionProvider: FirProjectSessionProvider,
      sessionConfigurator: FirSessionConfigurator.() -> Unit,
      createFirSession:
          (
              List<F>,
              FirModuleData,
              FirProjectSessionProvider,
              FirSessionConfigurator.() -> Unit,
          ) -> FirSession,
  ): SessionWithSources<F> {
    val platformModuleData =
        FirModuleDataImpl(
            rootModuleName,
            libraryList.regularDependencies,
            libraryList.dependsOnDependencies,
            libraryList.friendsDependencies,
            targetPlatform,
        )

    val session =
        createFirSession(files, platformModuleData, sessionProvider) { sessionConfigurator() }
    return SessionWithSources(session, files)
  }
}
