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

import com.facebook.buck.jvm.kotlin.compilerplugins.common.isStub
import java.io.ByteArrayOutputStream
import java.io.DataOutputStream
import java.io.File
import java.util.LinkedHashMap
import org.jetbrains.kotlin.GeneratedDeclarationKey
import org.jetbrains.kotlin.backend.common.extensions.IrGenerationExtension
import org.jetbrains.kotlin.backend.common.extensions.IrPluginContext
import org.jetbrains.kotlin.backend.jvm.JvmIrDeserializerImpl
import org.jetbrains.kotlin.cli.common.SessionWithSources
import org.jetbrains.kotlin.cli.common.config.KotlinSourceRoot
import org.jetbrains.kotlin.cli.common.config.kotlinSourceRoots
import org.jetbrains.kotlin.cli.common.messages.CompilerMessageLocation
import org.jetbrains.kotlin.cli.common.messages.CompilerMessageSeverity
import org.jetbrains.kotlin.cli.common.messages.MessageCollector
import org.jetbrains.kotlin.cli.jvm.compiler.EnvironmentConfigFiles
import org.jetbrains.kotlin.cli.jvm.compiler.KotlinCoreEnvironment
import org.jetbrains.kotlin.cli.jvm.compiler.VfsBasedProjectEnvironment
import org.jetbrains.kotlin.cli.jvm.compiler.createContextForIncrementalCompilation
import org.jetbrains.kotlin.cli.jvm.compiler.createLibraryListForJvm
import org.jetbrains.kotlin.cli.jvm.compiler.pipeline.ModuleCompilerEnvironment
import org.jetbrains.kotlin.cli.jvm.compiler.pipeline.ModuleCompilerIrBackendInput
import org.jetbrains.kotlin.cli.jvm.compiler.pipeline.ModuleCompilerOutput
import org.jetbrains.kotlin.cli.jvm.compiler.pipeline.convertToIrAndActualizeForJvm
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
import org.jetbrains.kotlin.descriptors.ClassKind
import org.jetbrains.kotlin.descriptors.EffectiveVisibility
import org.jetbrains.kotlin.descriptors.Modality
import org.jetbrains.kotlin.descriptors.Visibilities
import org.jetbrains.kotlin.diagnostics.DiagnosticReporterFactory
import org.jetbrains.kotlin.diagnostics.impl.BaseDiagnosticsCollector
import org.jetbrains.kotlin.extensions.CompilerConfigurationExtension
import org.jetbrains.kotlin.extensions.PreprocessedFileCreator
import org.jetbrains.kotlin.fir.DependencyListForCliModule
import org.jetbrains.kotlin.fir.FirElement
import org.jetbrains.kotlin.fir.FirModuleData
import org.jetbrains.kotlin.fir.FirModuleDataImpl
import org.jetbrains.kotlin.fir.FirSession
import org.jetbrains.kotlin.fir.backend.jvm.JvmFir2IrExtensions
import org.jetbrains.kotlin.fir.declarations.FirRegularClass
import org.jetbrains.kotlin.fir.declarations.FirValueParameter
import org.jetbrains.kotlin.fir.declarations.impl.FirResolvedDeclarationStatusImpl
import org.jetbrains.kotlin.fir.expressions.FirAnnotationCall
import org.jetbrains.kotlin.fir.expressions.FirErrorExpression
import org.jetbrains.kotlin.fir.expressions.FirExpression
import org.jetbrains.kotlin.fir.expressions.FirNamedArgumentExpression
import org.jetbrains.kotlin.fir.expressions.FirQualifiedAccessExpression
import org.jetbrains.kotlin.fir.expressions.FirVarargArgumentsExpression
import org.jetbrains.kotlin.fir.expressions.FirWrappedArgumentExpression
import org.jetbrains.kotlin.fir.expressions.builder.buildLiteralExpression
import org.jetbrains.kotlin.fir.expressions.builder.buildNamedArgumentExpression
import org.jetbrains.kotlin.fir.expressions.impl.FirResolvedArgumentList
import org.jetbrains.kotlin.fir.extensions.ExperimentalTopLevelDeclarationsGenerationApi
import org.jetbrains.kotlin.fir.extensions.FirAnalysisHandlerExtension
import org.jetbrains.kotlin.fir.extensions.FirDeclarationGenerationExtension
import org.jetbrains.kotlin.fir.extensions.FirExtensionRegistrar
import org.jetbrains.kotlin.fir.extensions.FirExtensionSessionComponent
import org.jetbrains.kotlin.fir.extensions.MemberGenerationContext
import org.jetbrains.kotlin.fir.java.FirProjectSessionProvider
import org.jetbrains.kotlin.fir.moduleData
import org.jetbrains.kotlin.fir.pipeline.FirResult
import org.jetbrains.kotlin.fir.pipeline.buildResolveAndCheckFirFromKtFiles
import org.jetbrains.kotlin.fir.plugin.createMemberProperty
import org.jetbrains.kotlin.fir.plugin.createTopLevelClass
import org.jetbrains.kotlin.fir.resolve.providers.dependenciesSymbolProvider
import org.jetbrains.kotlin.fir.session.FirJvmIncrementalCompilationSymbolProviders
import org.jetbrains.kotlin.fir.session.FirJvmSessionFactory
import org.jetbrains.kotlin.fir.session.FirSessionConfigurator
import org.jetbrains.kotlin.fir.session.FirSharableJavaComponents
import org.jetbrains.kotlin.fir.session.IncrementalCompilationContext
import org.jetbrains.kotlin.fir.session.createSymbolProviders
import org.jetbrains.kotlin.fir.session.environment.AbstractProjectFileSearchScope
import org.jetbrains.kotlin.fir.session.firCachesFactoryForCliMode
import org.jetbrains.kotlin.fir.symbols.SymbolInternals
import org.jetbrains.kotlin.fir.symbols.impl.FirCallableSymbol
import org.jetbrains.kotlin.fir.symbols.impl.FirClassLikeSymbol
import org.jetbrains.kotlin.fir.symbols.impl.FirClassSymbol
import org.jetbrains.kotlin.fir.symbols.impl.FirEnumEntrySymbol
import org.jetbrains.kotlin.fir.symbols.impl.FirPropertySymbol
import org.jetbrains.kotlin.fir.types.ConeErrorType
import org.jetbrains.kotlin.fir.types.coneType
import org.jetbrains.kotlin.fir.types.isBoolean
import org.jetbrains.kotlin.fir.types.isInt
import org.jetbrains.kotlin.fir.types.isLong
import org.jetbrains.kotlin.fir.types.isString
import org.jetbrains.kotlin.fir.types.resolvedType
import org.jetbrains.kotlin.fir.visitors.FirDefaultVisitorVoid
import org.jetbrains.kotlin.idea.KotlinFileType
import org.jetbrains.kotlin.ir.IrBuiltIns
import org.jetbrains.kotlin.ir.IrElement
import org.jetbrains.kotlin.ir.IrStatement
import org.jetbrains.kotlin.ir.declarations.IrAnonymousInitializer
import org.jetbrains.kotlin.ir.declarations.IrClass
import org.jetbrains.kotlin.ir.declarations.IrConstructor
import org.jetbrains.kotlin.ir.declarations.IrDeclarationBase
import org.jetbrains.kotlin.ir.declarations.IrDeclarationContainer
import org.jetbrains.kotlin.ir.declarations.IrDeclarationOrigin.GeneratedByPlugin
import org.jetbrains.kotlin.ir.declarations.IrDeclarationWithVisibility
import org.jetbrains.kotlin.ir.declarations.IrFactory
import org.jetbrains.kotlin.ir.declarations.IrField
import org.jetbrains.kotlin.ir.declarations.IrFile
import org.jetbrains.kotlin.ir.declarations.IrModuleFragment
import org.jetbrains.kotlin.ir.declarations.IrProperty
import org.jetbrains.kotlin.ir.declarations.IrSimpleFunction
import org.jetbrains.kotlin.ir.expressions.IrBody
import org.jetbrains.kotlin.ir.expressions.IrCall
import org.jetbrains.kotlin.ir.expressions.IrExpression
import org.jetbrains.kotlin.ir.expressions.IrExpressionBody
import org.jetbrains.kotlin.ir.expressions.IrReturn
import org.jetbrains.kotlin.ir.expressions.impl.IrConstImpl
import org.jetbrains.kotlin.ir.expressions.impl.IrReturnImpl
import org.jetbrains.kotlin.ir.symbols.IrReturnTargetSymbol
import org.jetbrains.kotlin.ir.symbols.UnsafeDuringIrConstructionAPI
import org.jetbrains.kotlin.ir.types.IrSimpleType
import org.jetbrains.kotlin.ir.types.IrType
import org.jetbrains.kotlin.ir.types.makeNullable
import org.jetbrains.kotlin.ir.visitors.IrElementTransformer
import org.jetbrains.kotlin.ir.visitors.IrElementVisitorVoid
import org.jetbrains.kotlin.metadata.jvm.JvmModuleProtoBuf
import org.jetbrains.kotlin.metadata.jvm.deserialization.JvmMetadataVersion
import org.jetbrains.kotlin.modules.Module
import org.jetbrains.kotlin.modules.TargetId
import org.jetbrains.kotlin.name.CallableId
import org.jetbrains.kotlin.name.ClassId
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.name.Name
import org.jetbrains.kotlin.platform.TargetPlatform
import org.jetbrains.kotlin.platform.jvm.JvmPlatforms
import org.jetbrains.kotlin.psi.KtAnnotationEntry
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.kotlin.psi.KtTreeVisitorVoid
import org.jetbrains.kotlin.resolve.multiplatform.hmppModuleName
import org.jetbrains.kotlin.resolve.multiplatform.isCommonSource
import org.jetbrains.kotlin.types.ConstantValueKind

data object JvmAbiGenPlugin : GeneratedDeclarationKey()

@SuppressWarnings("PackageLocationMismatch")
class K2JvmAbiFirAnalysisHandlerExtension(private val outputPath: String) :
    FirAnalysisHandlerExtension() {

  override fun isApplicable(configuration: CompilerConfiguration): Boolean {
    return true
  }

  override fun doAnalysis(project: Project, configuration: CompilerConfiguration): Boolean {
    val updatedConfiguration =
        configuration.copy().apply {
          put(JVMConfigurationKeys.RETAIN_OUTPUT_IN_MEMORY, false)
          put(JVMConfigurationKeys.OUTPUT_DIRECTORY, File(outputPath))
          put(JVMConfigurationKeys.VALIDATE_BYTECODE, true)
          put(JVMConfigurationKeys.SKIP_BODIES, true)
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
          sourceFiles,
      )
    } finally {
      Disposer.dispose(disposable)
    }
    return true
  }

  fun convertAnalyzedFirToIr(
      configuration: CompilerConfiguration,
      targetId: TargetId,
      analysisResults: FirResult,
      environment: ModuleCompilerEnvironment,
      sourceFiles: List<KtFile>,
  ): ModuleCompilerIrBackendInput {
    val extensions = JvmFir2IrExtensions(configuration, JvmIrDeserializerImpl())
    val (moduleFragment, components, pluginContext, irActualizedResult, _, symbolTable) =
        analysisResults.convertToIrAndActualizeForJvm(
            extensions,
            configuration,
            environment.diagnosticsReporter,
            listOf(NonAbiDeclarationsStrippingIrExtension(sourceFiles)),
        )

    return ModuleCompilerIrBackendInput(
        targetId,
        configuration,
        extensions,
        moduleFragment,
        components,
        pluginContext,
        irActualizedResult,
        symbolTable,
    )
  }

  // A backend entry point for Kosabi.
  private fun runBackendForKosabi(
      messageCollector: MessageCollector,
      projectEnvironment: VfsBasedProjectEnvironment,
      configuration: CompilerConfiguration,
      module: Module,
      analysisResults: FirResult,
      sourceFiles: List<KtFile>,
  ): ModuleCompilerOutput {
    // Ignore all FE errors
    val cleanDiagnosticReporter = DiagnosticReporterFactory.createPendingReporter(messageCollector)
    val compilerEnvironment = ModuleCompilerEnvironment(projectEnvironment, cleanDiagnosticReporter)

    // Transform FIR to replace error expressions in annotations with valid literals.
    // This is necessary because K2 FIR may fail to resolve const vals from K1-generated
    // stub JARs (source-only ABI), resulting in FirErrorExpression nodes that cause
    // FIR-to-IR conversion to crash.
    fixFirErrorExpressionsInAnnotations(analysisResults)

    val irInput =
        convertAnalyzedFirToIr(
            configuration,
            TargetId(module),
            analysisResults,
            compilerEnvironment,
            sourceFiles,
        )

    val result = generateCodeFromIr(irInput, compilerEnvironment)

    // Manually generate .kotlin_module file since SKIP_BODIES=true is set
    generateKotlinModuleFile(irInput.irModuleFragment, module.getModuleName(), configuration)

    return result
  }

  // Generate the .kotlin_module file manually
  // With SKIP_BODIES=true, the standard generateCodeFromIr doesn't produce a .kotlin_module file
  // because top-level properties/functions are not fully represented in the IR. However, the
  // file facade classes (*Kt.class) ARE generated and present in the IR, so we scan the IR
  // for them and build the module metadata.
  private fun generateKotlinModuleFile(
      moduleFragment: IrModuleFragment,
      moduleName: String,
      configuration: CompilerConfiguration,
  ) {
    val outputDir = configuration[JVMConfigurationKeys.OUTPUT_DIRECTORY] ?: return

    // Collect all packages that have Kotlin IR files
    val allKotlinPackages = mutableSetOf<String>()
    // Collect package-to-file-facade mappings by scanning IR for *Kt classes
    val packageToFileFacades = mutableMapOf<String, MutableSet<String>>()

    // Iterate through all IR files to:
    // 1. Track all packages (for files with only objects/classes)
    // 2. Find file facade classes (classes for top-level functions)
    for (irFile in moduleFragment.files) {
      val packageFqName = irFile.packageFqName.asString()
      allKotlinPackages.add(packageFqName)

      for (decl in irFile.declarations) {
        if (decl is IrClass) {
          // File facade classes are synthetic classes that hold top-level declarations.
          // They can be named:
          // 1. Default: <filename>Kt (e.g., "UtilsKt" for Utils.kt)
          // 2. Custom: @file:JvmName("CustomName") (e.g., "InsightsHostUtils")
          //
          // Since we're using SKIP_BODIES=true, we can't reliably check for functions.
          // Instead, we detect file facades by checking if the class:
          // - Is at file-level (already guaranteed by being in irFile.declarations)
          // - Is not a companion object
          // - Contains functions or appears to be a file facade
          //
          // The safest approach is to include all potential file facades.
          // Invalid entries are handled gracefully by the Kotlin compiler.
          val className = decl.name.asString()
          val isLikelyFileFacade =
              className.endsWith("Kt") || // Default file facade pattern
                  (!decl.isCompanion && // Not a companion object
                      !decl.isInner && // Not an inner class
                      decl.declarations.any { it is IrSimpleFunction }) // Has functions

          if (isLikelyFileFacade) {
            packageToFileFacades.getOrPut(packageFqName) { mutableSetOf() }.add(className)
          }
        }
      }
    }

    // Generate .kotlin_module if there are any Kotlin packages
    // even if they don't have file facades (e.g., files with only objects/classes)
    if (allKotlinPackages.isEmpty()) return

    // Build the Module protobuf
    val moduleBuilder = JvmModuleProtoBuf.Module.newBuilder()

    // Iterate over all packages (including those without file facades)
    for (packageFqName in allKotlinPackages.sorted()) {
      val packagePartsBuilder = JvmModuleProtoBuf.PackageParts.newBuilder()
      packagePartsBuilder.packageFqName = packageFqName

      // Add file facades if this package has any
      val fileFacades = packageToFileFacades[packageFqName]
      if (fileFacades != null) {
        for (fileFacade in fileFacades.sorted()) {
          packagePartsBuilder.addShortClassName(fileFacade)
        }
      }

      moduleBuilder.addPackageParts(packagePartsBuilder.build())
    }

    val moduleProto = moduleBuilder.build()

    // Serialize the module
    val version = JvmMetadataVersion.INSTANCE
    val versionArray = version.toArray()
    // 4KB initial buffer - aligns with memory page size and is sufficient for most .kotlin_module
    // files
    val baos = ByteArrayOutputStream(4096)
    val out = DataOutputStream(baos)
    out.writeInt(versionArray.size)
    for (number in versionArray) {
      out.writeInt(number)
    }
    // Write flags for Kotlin 1.4+
    out.writeInt(0)
    moduleProto.writeTo(out)
    out.flush()

    // Write to META-INF/<moduleName>.kotlin_module
    val metaInfDir = File(outputDir, "META-INF")
    metaInfDir.mkdirs()
    val moduleFile = File(metaInfDir, "$moduleName.kotlin_module")
    moduleFile.writeBytes(baos.toByteArray())
  }

  // Fix FirErrorExpression nodes in annotation arguments by replacing them with valid literals.
  // This is necessary because K2 FIR may fail to resolve const vals from K1-generated stub JARs
  // (source-only ABI dependencies). When this happens, the FIR contains FirErrorExpression nodes
  // which cause the FIR-to-IR conversion to crash. For ABI generation, we don't need the actual
  // values - we just need valid bytecode, so we replace error expressions with default literals.
  private fun fixFirErrorExpressionsInAnnotations(analysisResults: FirResult) {
    for (output in analysisResults.outputs) {
      for (firFile in output.fir) {
        firFile.accept(FirErrorExpressionFixerVisitor())
      }
    }
  }

  // Visitor that walks FIR trees and fixes error expressions in annotation arguments.
  // The FIR API is complex and mostly immutable, so we use a best-effort approach:
  // - For FirVarargArgumentsExpression, access the mutable arguments list via cast to impl
  // - For other expressions, visit children recursively
  private class FirErrorExpressionFixerVisitor : FirDefaultVisitorVoid() {
    override fun visitElement(element: FirElement) {
      // Visit all children
      element.acceptChildren(this)
    }

    override fun visitAnnotationCall(annotationCall: FirAnnotationCall) {
      // Fix error expressions in annotation arguments
      val argumentList = annotationCall.argumentList
      if (argumentList is FirResolvedArgumentList) {
        var needsRebuild = false
        // Check if any arguments contain errors that need fixing
        for ((argument, _) in argumentList.mapping) {
          if (hasErrorExpression(argument)) {
            needsRebuild = true
            break
          }
        }

        if (needsRebuild) {
          // Build a new argument list with errors replaced by literals
          // Use reflection to access the internal mapping and create a new list
          try {
            // Get the mapping field from FirResolvedArgumentList
            val mappingField = argumentList.javaClass.getDeclaredField("mapping")
            mappingField.isAccessible = true

            @Suppress("UNCHECKED_CAST")
            val oldMapping =
                mappingField.get(argumentList) as MutableMap<FirExpression, FirValueParameter>

            // Create a new mapping with fixed expressions
            val newMapping = LinkedHashMap<FirExpression, FirValueParameter>()
            for ((argument, param) in oldMapping) {
              val fixedArgument = fixOrReplaceArgument(argument)
              newMapping[fixedArgument] = param
            }

            // Replace the mapping
            oldMapping.clear()
            oldMapping.putAll(newMapping)
          } catch (_: Exception) {
            // If reflection fails, try the old approach
            for ((argument, _) in argumentList.mapping) {
              fixErrorExpressionsInArgument(argument)
            }
          }
        } else {
          // No errors to fix, just visit normally
          for ((argument, _) in argumentList.mapping) {
            fixErrorExpressionsInArgument(argument)
          }
        }
      }

      // Also visit any nested annotations
      super.visitAnnotationCall(annotationCall)
    }

    private fun fixErrorExpressionsInArgument(argument: FirElement) {
      when (argument) {
        is FirVarargArgumentsExpression -> {
          // VarargArgumentsExpression has a mutable list of arguments
          // We need to cast to the impl class to access the mutable list
          try {
            val argList = argument.arguments
            if (argList is MutableList<*>) {
              @Suppress("UNCHECKED_CAST") val mutableArgs = argList as MutableList<FirExpression>
              for (i in mutableArgs.indices) {
                val arg = mutableArgs[i]
                if (arg is FirErrorExpression) {
                  // Replace with a default literal (0 for Int)
                  mutableArgs[i] =
                      buildLiteralExpression(
                          source = null,
                          kind = ConstantValueKind.Int,
                          value = 0,
                          setType = true,
                      )
                } else {
                  // Recursively check nested expressions
                  fixErrorExpressionsInArgument(arg)
                }
              }
            }
          } catch (_: Exception) {
            // If we can't modify the list, skip this argument
          }
        }
        is FirNamedArgumentExpression -> {
          fixErrorExpressionsInArgument(argument.expression)
        }
        is FirWrappedArgumentExpression -> {
          fixErrorExpressionsInArgument(argument.expression)
        }
        else -> {
          // For other cases, visit children
          argument.acceptChildren(this)
        }
      }
    }

    private fun hasErrorExpression(element: FirElement): Boolean {
      return when (element) {
        is FirErrorExpression -> true
        is FirNamedArgumentExpression -> hasErrorExpression(element.expression)
        is FirWrappedArgumentExpression -> hasErrorExpression(element.expression)
        is FirVarargArgumentsExpression -> element.arguments.any { hasErrorExpression(it) }
        is FirQualifiedAccessExpression -> {
          // Check if the resolved type is an error type
          try {
            element.resolvedType is ConeErrorType
          } catch (_: Exception) {
            false
          }
        }
        else -> false
      }
    }

    private fun fixOrReplaceArgument(argument: FirExpression): FirExpression {
      return when (argument) {
        is FirErrorExpression -> buildDefaultLiteral(argument)
        is FirQualifiedAccessExpression -> {
          // If resolved type is error, replace with default literal
          try {
            if (argument.resolvedType is ConeErrorType) {
              buildDefaultLiteralFromExpression(argument)
            } else {
              argument
            }
          } catch (_: Exception) {
            argument
          }
        }
        is FirNamedArgumentExpression -> {
          if (hasErrorExpression(argument.expression)) {
            // Build a new named argument with the expression replaced
            buildNamedArgumentExpression {
              source = argument.source
              expression = fixOrReplaceArgument(argument.expression)
              isSpread = argument.isSpread
              name = argument.name
            }
          } else {
            argument
          }
        }
        is FirVarargArgumentsExpression -> {
          // Fix vararg arguments
          val argList = argument.arguments
          if (argList is MutableList<*>) {
            @Suppress("UNCHECKED_CAST") val mutableArgs = argList as MutableList<FirExpression>
            for (i in mutableArgs.indices) {
              val arg = mutableArgs[i]
              if (hasErrorExpression(arg)) {
                mutableArgs[i] = fixOrReplaceArgument(arg)
              }
            }
          }
          argument
        }
        else -> argument
      }
    }

    private fun buildDefaultLiteral(error: FirErrorExpression): FirExpression {
      // Try to determine the expected type from the error expression
      // For strings, use empty string; for numbers, use 0
      val typeRef = error.resolvedType
      val kind =
          when {
            typeRef.isString -> ConstantValueKind.String
            typeRef.isInt -> ConstantValueKind.Int
            typeRef.isLong -> ConstantValueKind.Long
            typeRef.isBoolean -> ConstantValueKind.Boolean
            else -> ConstantValueKind.String // Default to string for annotations
          }
      val value: Any =
          when (kind) {
            ConstantValueKind.String -> ""
            ConstantValueKind.Int -> 0
            ConstantValueKind.Long -> 0L
            ConstantValueKind.Boolean -> false
            else -> ""
          }
      return buildLiteralExpression(
          source = null,
          kind = kind,
          value = value,
          setType = true,
      )
    }

    private fun buildDefaultLiteralFromExpression(expression: FirExpression): FirExpression {
      // Build a default literal based on the expression's resolved type
      val typeRef = expression.resolvedType
      val kind =
          when {
            typeRef.isString -> ConstantValueKind.String
            typeRef.isInt -> ConstantValueKind.Int
            typeRef.isLong -> ConstantValueKind.Long
            typeRef.isBoolean -> ConstantValueKind.Boolean
            else -> ConstantValueKind.String // Default to string for annotations
          }
      val value: Any =
          when (kind) {
            ConstantValueKind.String -> ""
            ConstantValueKind.Int -> 0
            ConstantValueKind.Long -> 0L
            ConstantValueKind.Boolean -> false
            else -> ""
          }
      return buildLiteralExpression(
          source = null,
          kind = kind,
          value = value,
          setType = true,
      )
    }
  }

  private class NonAbiDeclarationsStrippingIrExtension(private val sourceFiles: List<KtFile>) :
      IrGenerationExtension {
    @OptIn(UnsafeDuringIrConstructionAPI::class)
    override fun generate(moduleFragment: IrModuleFragment, pluginContext: IrPluginContext) {
      // Filter out files generated from stubs, similar to K1 implementation
      val stubs = sourceFiles.filter { it.isStub() }
      val stubPaths: Set<String> = stubs.map { it.viewProvider.virtualFile.path }.toSet()

      // Remove IR files that were generated from stubs
      moduleFragment.files.removeAll { irFile -> stubPaths.contains(irFile.fileEntry.name) }

      // Remove plugin-generated declarations from all files
      moduleFragment.files.forEach { irFile ->
        irFile.declarations.removeAll { declaration ->
          (declaration.origin as? GeneratedByPlugin)?.pluginKey == JvmAbiGenPlugin
        }
      }

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

    override fun visitField(declaration: IrField, data: Nothing?): IrStatement {
      // For fields with initializers containing function calls,
      // replace the initializer with a default constant value.
      // This handles cases like: const val X = (10 * TimeConstants.MS_PER_SECOND).toInt()
      // where the initializer contains a function call that can't be evaluated at compile time
      // in source-only ABI mode.
      // We specifically check for IrCall to avoid replacing valid expressions like unary minus
      // (-1).
      val initializer = declaration.initializer
      if (initializer != null) {
        val expression = initializer.expression
        if (containsFunctionCalls(expression)) {
          // Replace with a default constant value based on the field type
          val defaultExpressionBody = generateDefaultExpressionBody(declaration.type)
          if (defaultExpressionBody != null) {
            declaration.initializer = defaultExpressionBody
          }
        }
      }
      return super.visitField(declaration, data)
    }

    // Check if an expression tree contains any function calls
    private fun containsFunctionCalls(expression: IrExpression): Boolean {
      if (expression is IrCall) return true
      var hasCall = false
      expression.acceptChildren(
          object : IrElementVisitorVoid {
            override fun visitElement(element: IrElement) {
              if (element is IrCall) hasCall = true
              element.acceptChildren(this, null)
            }
          },
          null,
      )
      return hasCall
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

  private class MissingConstantsVisitor(private val usedConstants: MutableSet<String>) :
      KtTreeVisitorVoid() {
    override fun visitAnnotationEntry(annotationEntry: KtAnnotationEntry) {
      // Use getArgumentExpression()?.text to get just the value, not the parameter name
      // For @Annotation(param = VALUE), we want "VALUE", not "param = VALUE"
      // For array expressions like [A, B], extract individual elements
      annotationEntry.valueArgumentList
          ?.arguments
          ?.mapNotNull { it.getArgumentExpression()?.text }
          ?.forEach { expressionText ->
            if (expressionText.startsWith("[") && expressionText.endsWith("]")) {
              // Array expression: parse and extract individual elements
              val arrayContent = expressionText.substring(1, expressionText.length - 1).trim()
              if (arrayContent.isNotEmpty()) {
                // Simple comma split - for annotation arguments, we don't expect complex nesting
                arrayContent.split(",").forEach { element ->
                  val trimmed = element.trim()
                  if (trimmed.isNotEmpty()) {
                    usedConstants.add(trimmed)
                  }
                }
              }
            } else {
              // Regular expression: add as-is
              usedConstants.add(expressionText)
            }
          }
      return super.visitAnnotationEntry(annotationEntry)
    }
  }

  // Collect missing constants from source files by analyzing imports
  private fun collectMissingConstantsFromSourceFiles(
      sourceFiles: List<KtFile>,
      session: FirSession,
  ): Map<ClassId, Set<String>> {
    val missingConstants = mutableMapOf<ClassId, MutableSet<String>>()

    // Collect all packages that are defined in the source files being compiled
    // to exclude constants from those packages (they're not "missing")
    val sourcePackages = sourceFiles.map { it.packageFqName }.toSet()

    for (sourceFile in sourceFiles) {
      val usedConstants = mutableSetOf<String>()
      sourceFile.accept(MissingConstantsVisitor(usedConstants))

      for (importDirective in sourceFile.importDirectives) {
        val importPath = importDirective.importedFqName ?: continue
        val segments = importPath.pathSegments()
        if (segments.size < 2) continue

        val importedName = importPath.shortNameOrSpecial().asString()

        // Check if this import is used in annotations
        // usedConstants contains things like "THREAD_TAG" or "TestConstants.THREAD_TAG"
        val matchingUsages = usedConstants.filter { it.contains(importedName) }
        if (matchingUsages.isEmpty()) continue

        // Determine if this is a class import or a constant import
        // For class imports like "import pkg.Class", usages will be "Class.PROPERTY"
        // For constant imports like "import pkg.Class.CONSTANT", usages will be "CONSTANT"
        for (usage in matchingUsages) {
          if (usage.startsWith("$importedName.")) {
            // Class import: import pkg.Class, used as Class.PROPERTY
            // Extract the property name from the usage
            val propertyPath = usage.removePrefix("$importedName.")
            // propertyPath could be "PROPERTY" or "Api.PROPERTY" (nested class)
            val pathParts = propertyPath.split(".")
            val propertyName = pathParts.last()

            // Build full segments including nested class parts
            // For import other_package.enums.VersionCodes, used as VersionCodes.Api.O:
            //   segments = [other_package, enums, VersionCodes]
            //   pathParts = [Api, O]
            //   nestedClassParts = [Api] (all except the property name)
            //   fullSegments = [other_package, enums, VersionCodes, Api]
            val nestedClassParts = pathParts.dropLast(1)
            val fullSegments = segments + nestedClassParts.map { Name.identifier(it) }

            // For class imports, the full import path IS the class
            val classId =
                findClassIdForImportWithProperty(
                    fullSegments,
                    propertyName,
                    session,
                    sourcePackages,
                    segments.size,
                )
            if (classId != null) {
              missingConstants.getOrPut(classId) { mutableSetOf() }.add(propertyName)
            }
          } else if (usage == importedName) {
            // Constant import: import pkg.Class.CONSTANT, used as CONSTANT
            val classSegments = segments.dropLast(1)
            val classId =
                findClassIdForImportWithProperty(
                    classSegments,
                    importedName,
                    session,
                    sourcePackages,
                    classSegments.size,
                )
            if (classId != null) {
              missingConstants.getOrPut(classId) { mutableSetOf() }.add(importedName)
            }
          }
        }
      }
    }

    return missingConstants
  }

  // Find the correct ClassId for an import path
  // For import other_package.constants.TestConstants.THREAD_TAG, segments would be
  // [other_package, constants, TestConstants] and we want ClassId(other_package.constants,
  // TestConstants)
  // Returns null if the class (or any parent class in the path) already exists in dependencies AND
  // has the property or enum entry
  @OptIn(SymbolInternals::class)
  private fun findClassIdForImportWithProperty(
      segments: List<Name>,
      propertyName: String,
      session: FirSession,
      sourcePackages: Set<FqName>,
      originalImportLength: Int,
  ): ClassId? {
    if (segments.size < 2) return null

    // Try different package/class splits to find if any class in the path exists in dependencies
    // For import like androidx.annotation.VisibleForTesting.Companion.PROTECTED (segments =
    // [androidx, annotation, VisibleForTesting, Companion])
    // We need to try:
    // - Package: androidx, Class: annotation (unlikely)
    // - Package: androidx.annotation, Class: VisibleForTesting (likely exists!)
    // - Package: androidx.annotation.VisibleForTesting, Class: Companion (won't exist as top-level)
    for (splitPoint in 1 until segments.size) {
      val packageFqName = FqName.fromSegments(segments.take(splitPoint).map { it.asString() })
      val classNameParts = segments.drop(splitPoint)

      // Skip if this is from a package being compiled
      if (packageFqName in sourcePackages) {
        continue
      }

      // Build the ClassId - for nested classes like VisibleForTesting.Companion,
      // we need to use the proper nested class syntax
      val topLevelClassName = classNameParts.first()
      val classId =
          if (classNameParts.size == 1) {
            ClassId(packageFqName, topLevelClassName)
          } else {
            // For nested classes, create ClassId with nested class path
            ClassId(packageFqName, FqName.fromSegments(classNameParts.map { it.asString() }), false)
          }

      val classSymbol = session.dependenciesSymbolProvider.getClassLikeSymbolByClassId(classId)
      if (classSymbol != null) {
        val firClassSymbol = classSymbol as? FirClassSymbol<*>
        if (firClassSymbol != null) {
          // Check for property with matching name
          val hasProperty =
              firClassSymbol.declarationSymbols.filterIsInstance<FirPropertySymbol>().any {
                it.name.asString() == propertyName
              }

          // Check for any callable with matching name (covers Java static fields)
          val hasCallable =
              firClassSymbol.declarationSymbols.filterIsInstance<FirCallableSymbol<*>>().any {
                it.name.asString() == propertyName
              }

          // Also check for enum entry with matching name
          val firRegularClass = firClassSymbol.fir as? FirRegularClass
          val hasEnumEntry =
              firRegularClass != null &&
                  firRegularClass.classKind == ClassKind.ENUM_CLASS &&
                  firClassSymbol.declarationSymbols.filterIsInstance<FirEnumEntrySymbol>().any {
                    it.name.asString() == propertyName
                  }

          // For annotation classes, constants are Java static fields which may not be
          // represented as FirPropertySymbol. Trust that the constant exists if the
          // annotation class is on the classpath.
          val isAnnotationClass =
              firRegularClass != null && firRegularClass.classKind == ClassKind.ANNOTATION_CLASS

          if (hasProperty || hasCallable || hasEnumEntry || isAnnotationClass) {
            // Found a class in dependencies that has the member - no stub needed
            return null
          }

          // Note: We intentionally do NOT check the Companion object here.
          // While companion object const vals are accessible as Class.PROPERTY in Kotlin,
          // K2 FIR cannot properly resolve them from deserialized stub JARs because:
          // 1. The bytecode has const vals as static fields on the outer class
          // 2. The Companion metadata describes the properties but has no accessor bytecode
          // 3. FIR resolution fails when trying to evaluate the constant value
          // By NOT returning null here, we allow the code to continue and potentially
          // generate a stub or handle the case differently.
        }
        // Class exists but doesn't have the property/entry - continue probing
      }
    }

    // No class found in dependencies with the property - generate a stub using heuristic
    // Use originalImportLength to determine package/class split
    // For import other_package.enums.VersionCodes (originalImportLength=3), used as
    // VersionCodes.Api.O:
    //   segments = [other_package, enums, VersionCodes, Api] (length=4, originalImportLength=3)
    //   packageFqName should be other_package.enums (first originalImportLength-1 segments)
    //   className should be VersionCodes.Api (remaining segments)
    val packageFqName =
        FqName.fromSegments(segments.take(originalImportLength - 1).map { it.asString() })
    val classNameParts = segments.drop(originalImportLength - 1)

    // Skip if from source package
    if (packageFqName in sourcePackages) {
      return null
    }

    // Build the ClassId with potential nested class path
    val topLevelClassName = classNameParts.first()
    val baseClassId =
        if (classNameParts.size == 1) {
          ClassId(packageFqName, topLevelClassName)
        } else {
          // For nested classes, create ClassId with nested class path
          ClassId(packageFqName, FqName.fromSegments(classNameParts.map { it.asString() }), false)
        }

    // If the class already exists in dependencies but doesn't have the property,
    // the property might be in its Companion object. Check if a Companion exists
    // and if so, generate the stub for the Companion class instead to avoid conflicts.
    val existingClass = session.dependenciesSymbolProvider.getClassLikeSymbolByClassId(baseClassId)
    if (existingClass != null) {
      val companionClassId = baseClassId.createNestedClassId(Name.identifier("Companion"))
      val existingCompanion =
          session.dependenciesSymbolProvider.getClassLikeSymbolByClassId(companionClassId)
      if (existingCompanion != null) {
        // Companion exists but doesn't have the property - generate stub in Companion
        return companionClassId
      }
      // Class exists but no Companion - can't generate stub without conflict
      // Return null to avoid generating conflicting class
      return null
    }

    return baseClassId
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
          val missingConstants = collectMissingConstantsFromSourceFiles(sources, session)
          session.jvmAbiGenService.state.missingConstants.putAll(missingConstants)
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

class AbiGenFirExtensionRegistrar() : FirExtensionRegistrar() {
  override fun ExtensionRegistrarContext.configurePlugin() {
    +{ session: FirSession -> MissingConstantDeclarationGenerationExtension(session) }
    +JvmAbiGenService.getFactory()
  }
}

@OptIn(ExperimentalTopLevelDeclarationsGenerationApi::class)
class MissingConstantDeclarationGenerationExtension(
    session: FirSession,
) : FirDeclarationGenerationExtension(session) {

  override fun getTopLevelClassIds(): Set<ClassId> {
    val missingConstants = session.jvmAbiGenService.state.missingConstants
    return missingConstants.keys.toSet()
  }

  override fun getTopLevelCallableIds(): Set<CallableId> {
    val missingConstants = session.jvmAbiGenService.state.missingConstants
    return missingConstants
        .flatMap { (classId, constants) ->
          constants.map { constantName -> CallableId(classId, Name.identifier(constantName)) }
        }
        .toSet()
  }

  override fun generateTopLevelClassLikeDeclaration(classId: ClassId): FirClassLikeSymbol<*>? {
    val missingConstants = session.jvmAbiGenService.state.missingConstants

    // Only generate if this class has missing constants
    val constantsForClass = missingConstants[classId] ?: return null

    val createdClass = createTopLevelClass(classId, JvmAbiGenPlugin, ClassKind.OBJECT)
    createdClass.replaceStatus(
        FirResolvedDeclarationStatusImpl(
                Visibilities.Public,
                Modality.FINAL,
                EffectiveVisibility.Public,
            )
            .apply { isStatic = true }
    )
    return createdClass.symbol
  }

  override fun getCallableNamesForClass(
      classSymbol: FirClassSymbol<*>,
      context: MemberGenerationContext,
  ): Set<Name> {
    val classId = classSymbol.classId
    val missingConstants = session.jvmAbiGenService.state.missingConstants
    val constantsForClass = missingConstants[classId] ?: return emptySet()

    return constantsForClass.map { Name.identifier(it) }.toSet()
  }

  override fun generateProperties(
      callableId: CallableId,
      context: MemberGenerationContext?,
  ): List<FirPropertySymbol> {
    val owner = context?.owner ?: return emptyList()
    val classId = callableId.classId ?: return emptyList()

    val missingConstants = session.jvmAbiGenService.state.missingConstants
    val constantsForClass = missingConstants[classId] ?: return emptyList()

    val constantName = callableId.callableName.asString()
    if (constantName !in constantsForClass) {
      return emptyList()
    }

    return listOf(generateConstantProperty(constantName, owner))
  }

  override fun hasPackage(packageFqName: FqName): Boolean {
    val missingConstants = session.jvmAbiGenService.state.missingConstants

    // Return true for any package that contains classes we're generating
    return missingConstants.keys.any { classId -> classId.packageFqName == packageFqName }
  }

  private fun generateConstantProperty(
      constantName: String,
      owner: FirClassSymbol<*>,
  ): FirPropertySymbol {
    // Default to String type with empty string value
    // Only constants that are not available in dependencies are generated
    val property =
        createMemberProperty(
            owner,
            JvmAbiGenPlugin,
            Name.identifier(constantName),
            session.builtinTypes.stringType.coneType,
        )
    property.replaceStatus(
        FirResolvedDeclarationStatusImpl(
                Visibilities.Public,
                Modality.FINAL,
                EffectiveVisibility.Public,
            )
            .apply { isConst = true }
    )
    property.replaceInitializer(
        buildLiteralExpression(
            source = null,
            kind = ConstantValueKind.String,
            value = "",
            setType = true,
        )
    )
    return property.symbol
  }
}

class JvmAbiGenService(session: FirSession, state: AbiGenState) :
    FirExtensionSessionComponent(session) {
  companion object {
    fun getFactory(): Factory {
      return Factory { JvmAbiGenService(it, AbiGenState()) }
    }
  }

  val state: AbiGenState = state
}

class AbiGenState {
  val missingConstants: MutableMap<ClassId, Set<String>> = mutableMapOf()
}

val FirSession.jvmAbiGenService: JvmAbiGenService by FirSession.sessionComponentAccessor()
