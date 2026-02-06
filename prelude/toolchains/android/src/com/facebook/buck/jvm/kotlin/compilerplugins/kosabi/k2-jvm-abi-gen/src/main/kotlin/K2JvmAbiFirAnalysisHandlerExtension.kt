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
import org.jetbrains.kotlin.extensions.CompilerConfigurationExtension
import org.jetbrains.kotlin.extensions.PreprocessedFileCreator
import org.jetbrains.kotlin.fir.DependencyListForCliModule
import org.jetbrains.kotlin.fir.FirElement
import org.jetbrains.kotlin.fir.FirModuleData
import org.jetbrains.kotlin.fir.FirModuleDataImpl
import org.jetbrains.kotlin.fir.FirSession
import org.jetbrains.kotlin.fir.backend.FirMetadataSource
import org.jetbrains.kotlin.fir.backend.jvm.JvmFir2IrExtensions
import org.jetbrains.kotlin.fir.declarations.FirRegularClass
import org.jetbrains.kotlin.fir.declarations.impl.FirResolvedDeclarationStatusImpl
import org.jetbrains.kotlin.fir.declarations.utils.isConst
import org.jetbrains.kotlin.fir.expressions.FirAnnotation
import org.jetbrains.kotlin.fir.expressions.FirAnnotationCall
import org.jetbrains.kotlin.fir.expressions.FirErrorExpression
import org.jetbrains.kotlin.fir.expressions.FirGetClassCall
import org.jetbrains.kotlin.fir.expressions.FirNamedArgumentExpression
import org.jetbrains.kotlin.fir.expressions.FirQualifiedAccessExpression
import org.jetbrains.kotlin.fir.expressions.FirVarargArgumentsExpression
import org.jetbrains.kotlin.fir.expressions.FirWrappedArgumentExpression
import org.jetbrains.kotlin.fir.expressions.builder.buildLiteralExpression
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
import org.jetbrains.kotlin.fir.pipeline.ModuleCompilerAnalyzedOutput
import org.jetbrains.kotlin.fir.pipeline.buildFirFromKtFiles
import org.jetbrains.kotlin.fir.pipeline.runResolution
import org.jetbrains.kotlin.fir.plugin.createMemberProperty
import org.jetbrains.kotlin.fir.plugin.createTopLevelClass
import org.jetbrains.kotlin.fir.references.FirErrorNamedReference
import org.jetbrains.kotlin.fir.references.FirResolvedNamedReference
import org.jetbrains.kotlin.fir.resolve.providers.dependenciesSymbolProvider
import org.jetbrains.kotlin.fir.resolve.providers.symbolProvider
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
import org.jetbrains.kotlin.ir.declarations.IrDeclarationWithVisibility
import org.jetbrains.kotlin.ir.declarations.IrFactory
import org.jetbrains.kotlin.ir.declarations.IrField
import org.jetbrains.kotlin.ir.declarations.IrFile
import org.jetbrains.kotlin.ir.declarations.IrMetadataSourceOwner
import org.jetbrains.kotlin.ir.declarations.IrModuleFragment
import org.jetbrains.kotlin.ir.declarations.IrProperty
import org.jetbrains.kotlin.ir.declarations.IrSimpleFunction
import org.jetbrains.kotlin.ir.expressions.IrBody
import org.jetbrains.kotlin.ir.expressions.IrCall
import org.jetbrains.kotlin.ir.expressions.IrClassReference
import org.jetbrains.kotlin.ir.expressions.IrConstructorCall
import org.jetbrains.kotlin.ir.expressions.IrExpression
import org.jetbrains.kotlin.ir.expressions.IrExpressionBody
import org.jetbrains.kotlin.ir.expressions.IrGetEnumValue
import org.jetbrains.kotlin.ir.expressions.IrReturn
import org.jetbrains.kotlin.ir.expressions.IrVararg
import org.jetbrains.kotlin.ir.expressions.impl.IrConstImpl
import org.jetbrains.kotlin.ir.expressions.impl.IrReturnImpl
import org.jetbrains.kotlin.ir.symbols.IrReturnTargetSymbol
import org.jetbrains.kotlin.ir.symbols.UnsafeDuringIrConstructionAPI
import org.jetbrains.kotlin.ir.types.IrErrorType
import org.jetbrains.kotlin.ir.types.IrSimpleType
import org.jetbrains.kotlin.ir.types.IrType
import org.jetbrains.kotlin.ir.types.makeNullable
import org.jetbrains.kotlin.ir.util.kotlinFqName
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
import org.jetbrains.org.objectweb.asm.AnnotationVisitor
import org.jetbrains.org.objectweb.asm.ClassReader
import org.jetbrains.org.objectweb.asm.ClassVisitor
import org.jetbrains.org.objectweb.asm.ClassWriter
import org.jetbrains.org.objectweb.asm.FieldVisitor
import org.jetbrains.org.objectweb.asm.MethodVisitor
import org.jetbrains.org.objectweb.asm.Opcodes

data object JvmAbiGenPlugin : GeneratedDeclarationKey()

@SuppressWarnings("PackageLocationMismatch")
class K2JvmAbiFirAnalysisHandlerExtension(private val outputPath: String) :
    FirAnalysisHandlerExtension() {

  // Store missing IR classes detected from first FIR run.
  // This must be class-level because each FIR run creates a new session.
  private val detectedMissingIrClasses = mutableSetOf<ClassId>()

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

      // Run FIR frontend once
      val initialAnalysisResults =
          runFrontendForKosabi(
              projectEnvironment,
              updatedConfiguration,
              sourceFiles,
              module,
          )

      // Detect missing transitive dependencies by checking if all supertypes can be resolved
      val missingTypes = collectMissingTypesFromFir(initialAnalysisResults)

      // If there are missing types, store them class-level and re-run FIR frontend
      val analysisResults =
          if (missingTypes.isNotEmpty()) {
            // Store in class-level field so it survives session recreation
            detectedMissingIrClasses.addAll(missingTypes)

            // Re-run frontend - pass the missing types explicitly
            runFrontendForKosabi(
                projectEnvironment,
                updatedConfiguration,
                sourceFiles,
                module,
                detectedMissingIrClasses,
            )
          } else {
            initialAnalysisResults
          }

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

  // Collect missing transitive dependencies by checking if supertypes can be resolved.
  // When FIR loads a class from bytecode (e.g., from a stub JAR), it includes references
  // to that class's supertypes. If those supertypes are not on the classpath (because
  // they're transitive dependencies not included in the stub JAR), we detect them here.
  private fun collectMissingTypesFromFir(analysisResults: FirResult): Set<ClassId> {
    val missingTypes = mutableSetOf<ClassId>()

    for (output in analysisResults.outputs) {
      val session = output.session

      for (firFile in output.fir) {
        firFile.accept(MissingTypeCollectorVisitor(session, missingTypes))
      }
    }

    return missingTypes
  }

  // Visitor that walks FIR and checks if all referenced types can be resolved,
  // including the entire supertype hierarchy of dependency classes
  @OptIn(SymbolInternals::class)
  private class MissingTypeCollectorVisitor(
      private val session: FirSession,
      private val missingTypes: MutableSet<ClassId>,
  ) : FirDefaultVisitorVoid() {

    // Track already-checked classes to avoid infinite recursion
    private val checkedClasses = mutableSetOf<ClassId>()

    override fun visitElement(element: FirElement) {
      // Only visit declarations, not expressions/bodies (avoid deep recursion)
      // We only care about type references in declarations
    }

    override fun visitFile(file: org.jetbrains.kotlin.fir.declarations.FirFile) {
      // Visit top-level declarations
      for (declaration in file.declarations) {
        declaration.accept(this)
      }
    }

    override fun visitRegularClass(regularClass: FirRegularClass) {
      // Check if all supertypes can be resolved, including their supertypes
      for (superTypeRef in regularClass.superTypeRefs) {
        // Start with isFromDependencyChain=false - we're in a source class
        // Once we resolve a dependency class, we'll set it to true
        checkTypeResolvableRecursively(superTypeRef.coneType, isFromDependencyChain = false)
      }
      // Visit nested declarations (but not bodies)
      for (declaration in regularClass.declarations) {
        declaration.accept(this)
      }
    }

    override fun visitSimpleFunction(
        simpleFunction: org.jetbrains.kotlin.fir.declarations.FirSimpleFunction
    ) {
      // Check return type - these are from source, so start with isFromDependencyChain=false
      checkTypeResolvableRecursively(
          simpleFunction.returnTypeRef.coneType,
          isFromDependencyChain = false,
      )

      // Check parameter types
      for (valueParameter in simpleFunction.valueParameters) {
        checkTypeResolvableRecursively(
            valueParameter.returnTypeRef.coneType,
            isFromDependencyChain = false,
        )
      }
      // Do NOT call super - we don't need to visit function bodies
    }

    override fun visitProperty(property: org.jetbrains.kotlin.fir.declarations.FirProperty) {
      // Check property type
      checkTypeResolvableRecursively(property.returnTypeRef.coneType, isFromDependencyChain = false)
      // Do NOT call super - we don't need to visit property initializers
    }

    override fun visitConstructor(
        constructor: org.jetbrains.kotlin.fir.declarations.FirConstructor
    ) {
      // Check parameter types
      for (valueParameter in constructor.valueParameters) {
        checkTypeResolvableRecursively(
            valueParameter.returnTypeRef.coneType,
            isFromDependencyChain = false,
        )
      }
      // Do NOT call super - we don't need to visit constructor bodies
    }

    // Check if a type can be resolved via the symbol provider, and recursively
    // check all supertypes of resolved classes. This is necessary because stub JARs
    // may contain classes that extend other classes not in the stub JAR.
    // The isFromDependencyChain parameter tracks whether we're checking a type that
    // was found in the supertype chain of a dependency class.
    private fun checkTypeResolvableRecursively(
        type: org.jetbrains.kotlin.fir.types.ConeKotlinType,
        isFromDependencyChain: Boolean = false,
    ) {
      when (type) {
        is org.jetbrains.kotlin.fir.types.ConeClassLikeType -> {
          val classId = type.lookupTag.classId

          // Skip if already checked
          if (classId in checkedClasses) return
          checkedClasses.add(classId)

          // Skip standard library classes - they're always available
          if (
              classId.packageFqName.asString().startsWith("kotlin") ||
                  classId.packageFqName.asString().startsWith("java")
          ) {
            return
          }

          // Skip local classes (anonymous classes, lambdas, object expressions)
          // These are local to the source file and can't be missing transitive dependencies.
          // Also, the symbol provider throws an error when trying to look them up.
          if (classId.isLocal) {
            return
          }

          // Try to resolve the class symbol from dependencies
          val depSymbol = session.dependenciesSymbolProvider.getClassLikeSymbolByClassId(classId)
          if (depSymbol == null) {
            // Not in dependencies - could be a source class (Kotlin or Java)
            // Try to resolve from all symbol providers (includes Java sources)
            val sourceSymbol = session.symbolProvider.getClassLikeSymbolByClassId(classId)
            if (sourceSymbol == null) {
              // Cannot be resolved at all
              if (isFromDependencyChain) {
                // This class is in the supertype chain of a dependency or source class
                // but cannot be resolved - it's a missing transitive dependency
                missingTypes.add(classId)
              }
              return
            } else {
              // Found in sources (Kotlin or Java) - recursively check its supertypes
              // When checking source class supertypes, we're now in a dependency chain
              // because any supertype not in sources must come from dependencies
              val firClass = sourceSymbol.fir as? FirRegularClass
              if (firClass != null) {
                for (superTypeRef in firClass.superTypeRefs) {
                  checkTypeResolvableRecursively(
                      superTypeRef.coneType,
                      isFromDependencyChain = true,
                  )
                }
              }
            }
          } else {
            // Class IS resolvable from dependencies - now recursively check ITS supertypes
            // These are now definitely from the dependency chain
            val firClass = depSymbol.fir as? FirRegularClass
            if (firClass != null) {
              for (superTypeRef in firClass.superTypeRefs) {
                checkTypeResolvableRecursively(superTypeRef.coneType, isFromDependencyChain = true)
              }
            }
          }
        }
        is org.jetbrains.kotlin.fir.types.ConeTypeParameterType -> {
          // Type parameters are always resolvable
        }
        is org.jetbrains.kotlin.fir.types.ConeFlexibleType -> {
          // Check both bounds
          checkTypeResolvableRecursively(type.lowerBound, isFromDependencyChain)
          checkTypeResolvableRecursively(type.upperBound, isFromDependencyChain)
        }
        else -> {
          // Other types don't need checking
        }
      }
    }
  }

  fun convertAnalyzedFirToIr(
      configuration: CompilerConfiguration,
      targetId: TargetId,
      analysisResults: FirResult,
      environment: ModuleCompilerEnvironment,
      sourceFiles: List<KtFile>,
      project: Project,
  ): ModuleCompilerIrBackendInput {
    val extensions = JvmFir2IrExtensions(configuration, JvmIrDeserializerImpl())
    // Include Parcelize IR extension to generate CREATOR field for @Parcelize classes.
    //
    // KNOWN LIMITATION: Parcelize crashes on @WriteWith<CustomParceler> when the parceler comes
    // from source-only-abi deps (missing override chain info). See T219106236.
    //
    // CURRENT WORKAROUND: Targets with custom parcelers need required_for_source_only_abi = True.
    //
    // RECOMMENDED LONG-TERM FIX: Replicate Parcelize generation inside K2 Kosabi (like K1 does):
    // - Don't run Parcelize IR extension at all
    // - Detect @Parcelize classes in IR after FIR-to-IR conversion
    // - Generate CREATOR field and method stubs ourselves
    // This avoids the @WriteWith issue entirely because we don't try to resolve parceler methods.
    //
    // WHY K1 DOESN'T HAVE THIS PROBLEM:
    // K1 Kosabi uses ClassBuilderMode.ABI with KotlinCodegenFacade.compileCorrectFiles().
    // In this mode, Parcelize generates declarations during codegen with method bodies
    // automatically stubbed - no need to resolve custom parceler methods.
    val registeredIrExtensions = IrGenerationExtension.getInstances(project)
    val abiSafeIrExtensions =
        registeredIrExtensions.filter { extension ->
          extension.javaClass.name.contains("parcelize", ignoreCase = true)
        }
    val allIrExtensions = abiSafeIrExtensions + NonAbiDeclarationsStrippingIrExtension(sourceFiles)
    val (moduleFragment, components, pluginContext, irActualizedResult, _, symbolTable) =
        analysisResults.convertToIrAndActualizeForJvm(
            extensions,
            configuration,
            environment.diagnosticsReporter,
            allIrExtensions,
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

    // Strip ALL annotations that have error expressions in their arguments.
    // This is necessary because K2 FIR may fail to resolve const vals or other references
    // from K1-generated stub JARs (source-only ABI), resulting in error expressions that
    // cause FIR-to-IR conversion to crash. K1 Kosabi naturally omits unresolvable annotations,
    // so we replicate that behavior. This is more robust than trying to replace error
    // expressions with literals (FIR mutation is unreliable).
    stripAnnotationsWithErrors(analysisResults)

    // Fix property initializers containing error expressions.
    // During FIR-to-IR conversion, constant evaluation of field initializers crashes
    // when encountering error expressions. We need to clear these initializers at the
    // FIR level before convertToIrAndActualizeForJvm is called.
    fixFirErrorExpressionsInPropertyInitializers(analysisResults)

    val irInput =
        convertAnalyzedFirToIr(
            configuration,
            TargetId(module),
            analysisResults,
            compilerEnvironment,
            sourceFiles,
            projectEnvironment.project,
        )

    // Strip all @Throws annotations from FIR metadata sources.
    // The IR still has @Throws annotations for bytecode generation (throws clause),
    // but the FIR metadata sources (used for metadata serialization) have them removed.
    // This prevents Safe Kotlin plugin from encountering @Throws during metadata reading.
    stripThrowsFromFirMetadataSources(irInput.irModuleFragment)

    // Strip ALL annotations with error expressions from FIR metadata sources.
    // This is necessary because annotations can persist into IR through FirMetadataSource
    // even after we've stripped them from FIR files. The IR constant evaluator will crash
    // if it encounters error expressions during annotation evaluation.
    stripAnnotationsWithErrorsFromFirMetadataSources(irInput.irModuleFragment)

    val result = generateCodeFromIr(irInput, compilerEnvironment)

    // Write bytecode from generationState.factory to disk.
    // With SKIP_BODIES=true, the output is kept in memory and not written automatically
    // by the standard pipeline. We need to manually extract and write each output file.
    val outputDir = configuration[JVMConfigurationKeys.OUTPUT_DIRECTORY]
    if (outputDir != null) {
      val outputFiles = result.generationState.factory.asList()
      outputFiles.forEach { outputFile ->
        val file = File(outputDir, outputFile.relativePath)
        file.parentFile?.mkdirs()
        file.writeBytes(outputFile.asByteArray())
      }
    }

    // Strip @Throws annotations from bytecode.
    // K2 JVM backend writes @Throws to both the throws clause AND RuntimeInvisibleAnnotations,
    // while K1 only writes the throws clause. Safe Kotlin reads from RuntimeInvisibleAnnotations
    // and complains even when exceptions are caught. We strip @Throws from bytecode annotations
    // while keeping the throws clause intact.
    if (outputDir != null) {
      stripThrowsAnnotationsFromBytecode(outputDir)
    }

    // Manually generate .kotlin_module file since SKIP_BODIES=true is set
    generateKotlinModuleFile(irInput.irModuleFragment, module.getModuleName(), configuration)

    return result
  }

  // Strip @Throws annotations from bytecode in all .class files.
  // K2 JVM backend writes @Throws to both the method's throws clause AND to
  // RuntimeInvisibleAnnotations, while K1 only writes the throws clause.
  // Safe Kotlin plugin reads @Throws from RuntimeInvisibleAnnotations and complains
  // even when exceptions are properly caught. We post-process the bytecode to remove
  // @Throws from annotations while keeping the throws clause intact.
  private fun stripThrowsAnnotationsFromBytecode(outputDir: File) {
    outputDir
        .walkTopDown()
        .filter { it.extension == "class" }
        .forEach { classFile ->
          val originalBytes = classFile.readBytes()
          val reader = ClassReader(originalBytes)
          val writer = ClassWriter(0) // Don't compute frames/maxs, just copy

          val visitor =
              object : ClassVisitor(Opcodes.ASM9, writer) {
                // Filter class-level annotations
                override fun visitAnnotation(
                    descriptor: String,
                    visible: Boolean,
                ): AnnotationVisitor? {
                  return if (isThrowsAnnotation(descriptor)) {
                    null // Skip @Throws annotation
                  } else {
                    super.visitAnnotation(descriptor, visible)
                  }
                }

                // Filter method-level annotations
                override fun visitMethod(
                    access: Int,
                    name: String,
                    descriptor: String,
                    signature: String?,
                    exceptions: Array<out String>?,
                ): MethodVisitor? {
                  val methodVisitor =
                      super.visitMethod(access, name, descriptor, signature, exceptions)
                  return if (methodVisitor != null) {
                    object : MethodVisitor(Opcodes.ASM9, methodVisitor) {
                      override fun visitAnnotation(
                          desc: String,
                          visible: Boolean,
                      ): AnnotationVisitor? {
                        return if (isThrowsAnnotation(desc)) {
                          null // Skip @Throws annotation
                        } else {
                          super.visitAnnotation(desc, visible)
                        }
                      }
                    }
                  } else {
                    null
                  }
                }

                // Filter field-level annotations
                override fun visitField(
                    access: Int,
                    name: String,
                    descriptor: String,
                    signature: String?,
                    value: Any?,
                ): FieldVisitor? {
                  val fieldVisitor = super.visitField(access, name, descriptor, signature, value)
                  return if (fieldVisitor != null) {
                    object : FieldVisitor(Opcodes.ASM9, fieldVisitor) {
                      override fun visitAnnotation(
                          desc: String,
                          visible: Boolean,
                      ): AnnotationVisitor? {
                        return if (isThrowsAnnotation(desc)) {
                          null // Skip @Throws annotation
                        } else {
                          super.visitAnnotation(desc, visible)
                        }
                      }
                    }
                  } else {
                    null
                  }
                }

                private fun isThrowsAnnotation(descriptor: String): Boolean {
                  return descriptor == "Lkotlin/jvm/Throws;" || descriptor == "Lkotlin/Throws;"
                }
              }

          reader.accept(visitor, 0)
          classFile.writeBytes(writer.toByteArray())
        }
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
                      decl.declarations.any {
                        it is IrSimpleFunction || it is IrProperty || it is IrField
                      }) // Has functions or properties

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

  // Strip ALL annotations that have error expressions in their arguments.
  // This is necessary because K2 FIR may fail to resolve const vals or other references
  // from K1-generated stub JARs (source-only ABI), resulting in error expressions that
  // cause FIR-to-IR conversion to crash. K1 Kosabi naturally omits unresolvable annotations,
  // so we replicate that behavior by stripping any annotation with errors.
  private fun stripAnnotationsWithErrors(analysisResults: FirResult) {
    for (output in analysisResults.outputs) {
      for (firFile in output.fir) {
        firFile.accept(FirAnnotationStrippingVisitor())
      }
    }
  }

  // Fix property initializers containing error expressions.
  // During FIR-to-IR conversion, constant evaluation fails when encountering error
  // expressions in property initializers, causing the compiler to crash.
  // For ABI generation, we only need property types, not the actual initializer values,
  // so we can safely clear initializers that contain errors.
  private fun fixFirErrorExpressionsInPropertyInitializers(analysisResults: FirResult) {
    for (output in analysisResults.outputs) {
      for (firFile in output.fir) {
        firFile.accept(FirPropertyInitializerFixerVisitor())
      }
    }
  }

  // Strip @Throws annotations with error types from FIR metadata sources attached to IR
  // declarations.
  // We only remove @Throws with error types to avoid breaking Java interop, while keeping valid
  // @Throws annotations so Safe Kotlin plugin can enforce exception handling correctly.
  private fun stripThrowsFromFirMetadataSources(moduleFragment: IrModuleFragment) {
    val THROWS_FQ_NAME = FqName("kotlin.jvm.Throws")
    val THROWS_KOTLIN_FQ_NAME = FqName("kotlin.Throws")

    moduleFragment.accept(
        object : IrElementVisitorVoid {
          override fun visitElement(element: IrElement) {
            element.acceptChildren(this, null)
          }

          override fun visitFile(file: IrFile) {
            // IrFile doesn't extend IrDeclarationBase, but we still want to process its
            // declarations
            super.visitFile(file)
          }

          override fun visitClass(declaration: IrClass) {
            stripThrowsFromDeclaration(declaration)
            super.visitClass(declaration)
          }

          override fun visitSimpleFunction(declaration: IrSimpleFunction) {
            stripThrowsFromDeclaration(declaration)
            super.visitSimpleFunction(declaration)
          }

          override fun visitProperty(declaration: IrProperty) {
            stripThrowsFromDeclaration(declaration)
            super.visitProperty(declaration)
          }

          override fun visitConstructor(declaration: IrConstructor) {
            stripThrowsFromDeclaration(declaration)
            super.visitConstructor(declaration)
          }

          private fun stripThrowsFromDeclaration(declaration: IrDeclarationBase) {
            // Access metadata via IrMetadataSourceOwner interface
            val metadataSourceOwner = declaration as? IrMetadataSourceOwner ?: return
            val metadataSource = metadataSourceOwner.metadata ?: return

            // Check if this is a FirMetadataSource
            val firMetadataSource = metadataSource as? FirMetadataSource ?: return

            // Strip @Throws from the FIR declaration
            stripThrowsFromFirDeclaration(firMetadataSource.fir)
          }

          private fun stripThrowsFromFirDeclaration(
              declaration: org.jetbrains.kotlin.fir.declarations.FirDeclaration?
          ) {
            if (declaration == null) return

            try {
              // Access the annotations field - it's of type MutableOrEmptyList<FirAnnotation>
              val annotationsField = declaration.javaClass.getDeclaredField("annotations")
              annotationsField.isAccessible = true
              val annotationsWrapper = annotationsField.get(declaration) ?: return

              // MutableOrEmptyList is a value class wrapping MutableList<T>?
              // Get the internal "list" field
              val listField = annotationsWrapper.javaClass.getDeclaredField("list")
              listField.isAccessible = true
              @Suppress("UNCHECKED_CAST")
              val annotations =
                  listField.get(annotationsWrapper) as? MutableList<FirAnnotation> ?: return

              // Only remove @Throws annotations that have error types in their exception class
              // arguments
              val toRemove =
                  annotations.filter { annotation ->
                    hasErrorTypeInThrowsAnnotation(
                        annotation,
                        THROWS_FQ_NAME,
                        THROWS_KOTLIN_FQ_NAME,
                    )
                  }

              if (toRemove.isNotEmpty()) {
                annotations.removeAll(toRemove)
              }
            } catch (_: Exception) {
              // If reflection fails, skip this declaration
            }
          }

          private fun hasErrorTypeInThrowsAnnotation(
              annotation: FirAnnotation,
              throwsFqName: FqName,
              throwsKotlinFqName: FqName,
          ): Boolean {
            // Check if this is @Throws annotation
            val annotationType = annotation.annotationTypeRef.coneType
            val fqName =
                (annotationType as? org.jetbrains.kotlin.fir.types.ConeClassLikeType)
                    ?.lookupTag
                    ?.classId
                    ?.asSingleFqName()

            if (fqName != throwsFqName && fqName != throwsKotlinFqName) {
              return false
            }

            // Check if any exception class argument is an error type
            // FirAnnotation doesn't have argumentList, only FirAnnotationCall does
            val annotationCall = annotation as? FirAnnotationCall ?: return false
            val argumentList = annotationCall.argumentList
            if (argumentList is FirResolvedArgumentList) {
              for ((argument, _) in argumentList.mapping) {
                if (hasErrorTypeInFirClassReference(argument)) {
                  return true
                }
              }
            }
            return false
          }

          private fun hasErrorTypeInFirClassReference(element: FirElement): Boolean {
            return when (element) {
              is FirVarargArgumentsExpression ->
                  element.arguments.any { hasErrorTypeInFirClassReference(it) }
              is FirGetClassCall -> {
                try {
                  val argument = element.argument
                  if (argument is FirQualifiedAccessExpression) {
                    argument.resolvedType is ConeErrorType
                  } else {
                    element.resolvedType is ConeErrorType
                  }
                } catch (_: Exception) {
                  false
                }
              }
              is FirQualifiedAccessExpression -> {
                try {
                  element.resolvedType is ConeErrorType ||
                      element.calleeReference is FirErrorNamedReference
                } catch (_: Exception) {
                  false
                }
              }
              is FirErrorExpression -> true
              else -> false
            }
          }
        },
        null,
    )
  }

  // Strip ALL annotations with error expressions from FIR metadata sources attached to IR
  // declarations. This is necessary because annotations persist into IR through FirMetadataSource,
  // and the IR constant evaluator crashes when encountering error expressions during annotation
  // evaluation. We strip any annotation that has error expressions in its arguments.
  private fun stripAnnotationsWithErrorsFromFirMetadataSources(moduleFragment: IrModuleFragment) {
    moduleFragment.accept(
        object : IrElementVisitorVoid {
          override fun visitElement(element: IrElement) {
            element.acceptChildren(this, null)
          }

          override fun visitFile(file: IrFile) {
            super.visitFile(file)
          }

          override fun visitClass(declaration: IrClass) {
            stripAnnotationsWithErrorsFromDeclaration(declaration)
            super.visitClass(declaration)
          }

          override fun visitSimpleFunction(declaration: IrSimpleFunction) {
            stripAnnotationsWithErrorsFromDeclaration(declaration)
            super.visitSimpleFunction(declaration)
          }

          override fun visitProperty(declaration: IrProperty) {
            stripAnnotationsWithErrorsFromDeclaration(declaration)
            super.visitProperty(declaration)
          }

          override fun visitConstructor(declaration: IrConstructor) {
            stripAnnotationsWithErrorsFromDeclaration(declaration)
            super.visitConstructor(declaration)
          }

          private fun stripAnnotationsWithErrorsFromDeclaration(declaration: IrDeclarationBase) {
            // Access metadata via IrMetadataSourceOwner interface
            val metadataSourceOwner = declaration as? IrMetadataSourceOwner ?: return
            val metadataSource = metadataSourceOwner.metadata ?: return

            // Check if this is a FirMetadataSource
            val firMetadataSource = metadataSource as? FirMetadataSource ?: return

            // Strip annotations with errors from the FIR declaration
            stripAnnotationsWithErrorsFromFirDeclaration(firMetadataSource.fir)
          }

          private fun stripAnnotationsWithErrorsFromFirDeclaration(
              declaration: org.jetbrains.kotlin.fir.declarations.FirDeclaration?
          ) {
            if (declaration == null) return

            try {
              // Access the annotations field
              val annotationsField = declaration.javaClass.getDeclaredField("annotations")
              annotationsField.isAccessible = true
              val annotationsWrapper = annotationsField.get(declaration) ?: return

              // MutableOrEmptyList is a value class wrapping MutableList<T>?
              // Get the internal "list" field
              val listField = annotationsWrapper.javaClass.getDeclaredField("list")
              listField.isAccessible = true
              @Suppress("UNCHECKED_CAST")
              val annotations =
                  listField.get(annotationsWrapper) as? MutableList<FirAnnotation> ?: return

              // Remove any annotation that has error expressions in its arguments
              val toRemove =
                  annotations.filter { annotation -> hasErrorExpressionInFirAnnotation(annotation) }

              if (toRemove.isNotEmpty()) {
                annotations.removeAll(toRemove)
              }
            } catch (_: Exception) {
              // If reflection fails, skip this declaration
            }
          }

          private fun hasErrorExpressionInFirAnnotation(annotation: FirAnnotation): Boolean {
            val annotationCall = annotation as? FirAnnotationCall ?: return false
            val argumentList = annotationCall.argumentList
            if (argumentList is FirResolvedArgumentList) {
              for ((argument, _) in argumentList.mapping) {
                if (hasErrorExpressionInFirElement(argument)) {
                  return true
                }
              }
            }
            return false
          }

          private fun hasErrorExpressionInFirElement(element: FirElement): Boolean {
            return when (element) {
              is FirErrorExpression -> true
              is FirNamedArgumentExpression -> hasErrorExpressionInFirElement(element.expression)
              is FirWrappedArgumentExpression -> hasErrorExpressionInFirElement(element.expression)
              is FirVarargArgumentsExpression ->
                  element.arguments.any { hasErrorExpressionInFirElement(it) }
              is FirQualifiedAccessExpression -> {
                try {
                  element.resolvedType is ConeErrorType ||
                      element.calleeReference is FirErrorNamedReference
                } catch (_: Exception) {
                  false
                }
              }
              is FirGetClassCall -> {
                try {
                  val argument = element.argument
                  if (argument is FirQualifiedAccessExpression) {
                    argument.resolvedType is ConeErrorType
                  } else {
                    element.resolvedType is ConeErrorType
                  }
                } catch (_: Exception) {
                  false
                }
              }
              is org.jetbrains.kotlin.fir.expressions.FirFunctionCall -> {
                try {
                  element.resolvedType is ConeErrorType ||
                      element.calleeReference is FirErrorNamedReference
                } catch (_: Exception) {
                  false
                }
              }
              else -> false
            }
          }
        },
        null,
    )
  }

  // Recursively check if a FIR element contains error expressions.
  // This detects both direct FirErrorExpression nodes and qualified accesses with error types.
  private fun hasErrorExpressionRecursive(element: FirElement): Boolean {
    return when (element) {
      is FirErrorExpression -> true
      is FirQualifiedAccessExpression -> {
        try {
          element.resolvedType is ConeErrorType || element.calleeReference is FirErrorNamedReference
        } catch (_: Exception) {
          false
        }
      }
      else -> {
        // Check children recursively
        var hasError = false
        element.acceptChildren(
            object : FirDefaultVisitorVoid() {
              override fun visitElement(childElement: FirElement) {
                if (hasErrorExpressionRecursive(childElement)) {
                  hasError = true
                }
                // Continue visiting children
                childElement.acceptChildren(this)
              }
            }
        )
        hasError
      }
    }
  }

  // Visitor that clears property initializers containing error expressions
  private inner class FirPropertyInitializerFixerVisitor : FirDefaultVisitorVoid() {
    override fun visitElement(element: FirElement) {
      element.acceptChildren(this)
    }

    override fun visitProperty(property: org.jetbrains.kotlin.fir.declarations.FirProperty) {
      val initializer = property.initializer
      if (initializer != null && hasErrorExpressionRecursive(initializer)) {
        // Clear the initializer using reflection
        try {
          val initializerField = property.javaClass.getDeclaredField("initializer")
          initializerField.isAccessible = true
          initializerField.set(property, null)
        } catch (_: Exception) {
          // If reflection fails, skip this property
        }
      }
      // Continue visiting nested declarations
      super.visitProperty(property)
    }
  }

  // Visitor that strips ALL annotations that have error expressions in their arguments.
  // This is more robust than trying to replace error expressions with literals,
  // because FIR mutation is unreliable. If an annotation can't be resolved, we remove it
  // entirely - for ABI generation, it's better to have no annotation than to crash.
  // This matches K1 kosabi behavior where unresolved annotations are naturally omitted.
  private class FirAnnotationStrippingVisitor : FirDefaultVisitorVoid() {
    override fun visitElement(element: FirElement) {
      // Strip annotations from any annotated element
      if (element is org.jetbrains.kotlin.fir.declarations.FirDeclaration) {
        stripAnnotationsWithErrors(element)
      }
      element.acceptChildren(this)
    }

    private fun stripAnnotationsWithErrors(
        declaration: org.jetbrains.kotlin.fir.declarations.FirDeclaration
    ) {
      try {
        // FIR annotations are mostly immutable, but we can access the mutable list via reflection
        val annotationsField = declaration.javaClass.getDeclaredField("annotations")
        annotationsField.isAccessible = true
        @Suppress("UNCHECKED_CAST")
        val annotations =
            annotationsField.get(declaration) as? MutableList<FirAnnotationCall> ?: return

        val toRemove =
            annotations.filter { annotation -> hasErrorExpressionInAnnotation(annotation) }

        if (toRemove.isNotEmpty()) {
          annotations.removeAll(toRemove)
        }
      } catch (e: Exception) {
        // If reflection fails, skip this declaration
      }
    }

    private fun hasErrorExpressionInAnnotation(annotation: FirAnnotationCall): Boolean {
      val argumentList = annotation.argumentList
      if (argumentList is FirResolvedArgumentList) {
        for ((argument, _) in argumentList.mapping) {
          if (hasErrorExpression(argument)) {
            return true
          }
        }
      }
      return false
    }

    @OptIn(SymbolInternals::class)
    private fun hasErrorExpression(element: FirElement): Boolean {
      return when (element) {
        is FirErrorExpression -> true
        is FirNamedArgumentExpression -> hasErrorExpression(element.expression)
        is FirWrappedArgumentExpression -> hasErrorExpression(element.expression)
        is org.jetbrains.kotlin.fir.expressions.FirSpreadArgumentExpression -> {
          // Spread operator (*array) - check the underlying expression
          hasErrorExpression(element.expression)
        }
        is FirVarargArgumentsExpression -> {
          // Vararg parameter - check all arguments
          element.arguments.any { hasErrorExpression(it) }
        }
        is FirQualifiedAccessExpression -> {
          // Check if the resolved type is an error type (covers unresolved enum values like
          // NO_RESTRICTION)
          // Also check if the callee reference itself is an error (catches cases where the type
          // is valid like Int, but the actual reference can't be resolved from stub JARs)
          try {
            if (
                element.resolvedType is ConeErrorType ||
                    element.calleeReference is FirErrorNamedReference
            ) {
              return true
            }

            // Check if this references a const val - these cannot be evaluated during IR
            // constant evaluation when building source-only ABI because the actual const value
            // is only available in stub JARs. This catches cases like UserConstants.NO_RESTRICTION
            // where the type (Int) is known and the property symbol is resolvable, but the
            // const value can't be evaluated.
            val calleeRef = element.calleeReference
            if (calleeRef is FirResolvedNamedReference) {
              val symbol = calleeRef.resolvedSymbol
              if (symbol is FirPropertySymbol) {
                val prop = symbol.fir
                // If it's a const val, only treat as error if the initializer:
                // 1. Is null (no value available)
                // 2. Contains error expressions (unresolvable)
                // Otherwise, the const val has a valid resolvable value.
                if (prop.isConst) {
                  val initializer = prop.initializer
                  if (initializer == null || hasErrorExpression(initializer)) {
                    return true
                  }
                  // The const val has a valid initializer, don't treat as error
                  return false
                }
              }
            }
            false
          } catch (_: Exception) {
            // If we can't access the type/reference due to an exception, treat it as an error
            // This catches cases where the element is in an inconsistent state
            true
          }
        }
        is FirGetClassCall -> {
          // For class references like IOException::class, check if the class type is an error type
          try {
            val argument = element.argument
            if (argument is FirQualifiedAccessExpression) {
              argument.resolvedType is ConeErrorType
            } else {
              element.resolvedType is ConeErrorType
            }
          } catch (_: Exception) {
            // Treat exceptions as errors
            true
          }
        }
        is org.jetbrains.kotlin.fir.expressions.FirArrayLiteral -> {
          // Array literal [a, b, c] - check all elements
          // FirArrayLiteral interface doesn't expose 'arguments' directly, use reflection
          try {
            val argumentListMethod = element.javaClass.getMethod("getArgumentList")
            val argumentList =
                argumentListMethod.invoke(element)
                    as? org.jetbrains.kotlin.fir.expressions.FirArgumentList
            argumentList?.arguments?.any { hasErrorExpression(it) } ?: false
          } catch (_: Exception) {
            // If reflection fails, treat as error to be safe
            true
          }
        }
        is org.jetbrains.kotlin.fir.expressions.FirFunctionCall -> {
          // Check if function call has error type (unresolved calls)
          try {
            element.resolvedType is ConeErrorType ||
                element.calleeReference is FirErrorNamedReference
          } catch (_: Exception) {
            // Treat exceptions as errors
            true
          }
        }
        else -> false
      }
    }
  }

  private class NonAbiDeclarationsStrippingIrExtension(private val sourceFiles: List<KtFile>) :
      IrGenerationExtension {

    private fun shouldStripAnnotation(annotation: IrConstructorCall): Boolean {
      val annotationClass = annotation.symbol.owner.parent as? IrClass ?: return false
      val annotationFqName = annotationClass.kotlinFqName.asString()

      // Keep @Throws annotation in IR to generate bytecode throws clause for Java interop.
      // The JVM backend (FunctionCodegen.getThrownExceptions) reads @Throws from IR annotations
      // to generate the method's throws clause. If we strip it here, Java code cannot catch
      // checked exceptions because the throws clause won't be in bytecode.
      // Note: K2 JVM backend writes @Throws to both throws clause AND RuntimeInvisibleAnnotations.
      // We strip it from RuntimeInvisibleAnnotations via bytecode post-processing (see
      // stripThrowsAnnotationsFromBytecode) to match K1 behavior and prevent Safe Kotlin errors.
      if (annotationFqName == "kotlin.jvm.Throws" || annotationFqName == "kotlin.Throws") {
        return false
      }

      // Strip SOURCE retention annotations (not needed in ABI)
      for (retentionAnnotation in annotationClass.annotations) {
        val retentionClass = retentionAnnotation.symbol.owner.parent as? IrClass ?: continue
        if (retentionClass.kotlinFqName.asString() == "kotlin.annotation.Retention") {
          if (retentionAnnotation.valueArgumentsCount > 0) {
            val arg = retentionAnnotation.getValueArgument(0)
            if (arg is IrGetEnumValue && arg.symbol.owner.name.asString() == "SOURCE") {
              return true
            }
          }
        }
      }
      return false
    }

    // Check if an annotation contains error types in its arguments.
    // Error types occur when K2 cannot resolve constants from source-only ABI dependencies.
    // These annotations cause crashes during constant evaluation, so we strip them.
    private fun hasErrorType(annotation: IrConstructorCall): Boolean {
      for (i in 0 until annotation.valueArgumentsCount) {
        val arg = annotation.getValueArgument(i)
        if (arg != null && containsErrorType(arg)) {
          return true
        }
      }
      return false
    }

    // Recursively check if an IR expression contains error types
    private fun containsErrorType(expression: IrExpression): Boolean {
      // Check if the expression's type is an error type
      if (expression.type is IrErrorType) {
        return true
      }

      // For class references (like IOException::class in @Throws), check if the referenced type
      // is an error type. This handles cases where @Throws has unresolved exception classes.
      if (expression is IrClassReference && expression.classType is IrErrorType) {
        return true
      }

      // For varargs (like @Throws(E1::class, E2::class)), check all elements
      if (expression is IrVararg) {
        for (element in expression.elements) {
          if (element is IrExpression && containsErrorType(element)) {
            return true
          }
        }
      }

      // Recursively check children
      var hasError = false
      expression.acceptChildren(
          object : IrElementVisitorVoid {
            override fun visitElement(element: IrElement) {
              if (element is IrExpression && element.type is IrErrorType) {
                hasError = true
              }
              // Also check class references in children
              if (element is IrClassReference && element.classType is IrErrorType) {
                hasError = true
              }
              element.acceptChildren(this, null)
            }
          },
          null,
      )
      return hasError
    }

    private fun stripSourceRetentionAnnotations(moduleFragment: IrModuleFragment) {
      moduleFragment.accept(
          object : IrElementVisitorVoid {
            override fun visitElement(element: IrElement) {
              element.acceptChildren(this, null)
            }

            override fun visitClass(declaration: IrClass) {
              declaration.annotations =
                  declaration.annotations.filter { !shouldStripAnnotation(it) && !hasErrorType(it) }
              super.visitClass(declaration)
            }

            override fun visitSimpleFunction(declaration: IrSimpleFunction) {
              declaration.annotations =
                  declaration.annotations.filter { !shouldStripAnnotation(it) && !hasErrorType(it) }
              declaration.valueParameters.forEach { param ->
                param.annotations =
                    param.annotations.filter { !shouldStripAnnotation(it) && !hasErrorType(it) }
              }
              super.visitSimpleFunction(declaration)
            }

            override fun visitField(declaration: IrField) {
              declaration.annotations =
                  declaration.annotations.filter { !shouldStripAnnotation(it) && !hasErrorType(it) }
              super.visitField(declaration)
            }

            override fun visitProperty(declaration: IrProperty) {
              declaration.annotations =
                  declaration.annotations.filter { !shouldStripAnnotation(it) && !hasErrorType(it) }
              super.visitProperty(declaration)
            }

            override fun visitConstructor(declaration: IrConstructor) {
              declaration.annotations =
                  declaration.annotations.filter { !shouldStripAnnotation(it) && !hasErrorType(it) }
              declaration.valueParameters.forEach { param ->
                param.annotations =
                    param.annotations.filter { !shouldStripAnnotation(it) && !hasErrorType(it) }
              }
              super.visitConstructor(declaration)
            }
          },
          null,
      )
    }

    @OptIn(UnsafeDuringIrConstructionAPI::class)
    override fun generate(moduleFragment: IrModuleFragment, pluginContext: IrPluginContext) {
      // Filter out files generated from stubs, similar to K1 implementation
      val stubs = sourceFiles.filter { it.isStub() }
      val stubPaths: Set<String> = stubs.map { it.viewProvider.virtualFile.path }.toSet()

      // Remove IR files that were generated from stubs
      moduleFragment.files.removeAll { irFile -> stubPaths.contains(irFile.fileEntry.name) }

      // Remove IR files for plugin-generated declarations (e.g., "__GENERATED DECLARATIONS__")
      // These files contain stub declarations generated by our
      // MissingConstantDeclarationGenerationExtension
      // for FIR resolution of missing constants and transitive dependencies. They're temporary
      // artifacts
      // used during FIR analysis and should not be included in the final bytecode.
      // Note: This filtering preserves legitimate plugin-generated methods like Parcelize's
      // describeContents()/writeToParcel() which are generated directly in the class.
      moduleFragment.files.removeAll { irFile ->
        irFile.fileEntry.name.contains("__GENERATED DECLARATIONS__") ||
            irFile.fileEntry.name.contains("GENERATED_DECLARATIONS")
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
          // For const vals, also verify the initializer is resolvable (not an error expression)
          // Source-only ABI JARs may have const vals with stripped initializers
          val hasResolvableProperty =
              firClassSymbol.declarationSymbols.filterIsInstance<FirPropertySymbol>().any { prop ->
                if (prop.name.asString() != propertyName) {
                  return@any false
                }

                // If it's not a const, it's usable as-is
                if (!prop.isConst) {
                  return@any true
                }

                // For const vals, check if we can actually resolve the initializer value
                // Const vals from source-only ABI JARs may exist but have no evaluable value
                val initializer = prop.fir.initializer
                initializer != null && !hasErrorExpressionRecursive(initializer)
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

          if (hasResolvableProperty || hasCallable || hasEnumEntry || isAnnotationClass) {
            // Found a class in dependencies that has the member - no stub needed
            return null
          }

          // Check companion object for const vals
          // Companion object const vals are accessible as Class.PROPERTY in Kotlin bytecode,
          // but from source-only ABI JARs, the initializer values may be stripped.
          // We need to check if the companion has the property AND if it's resolvable.
          val companionSymbol =
              firClassSymbol.declarationSymbols.filterIsInstance<FirClassSymbol<*>>().firstOrNull {
                it.name.asString() == "Companion"
              }
          if (companionSymbol != null) {
            val hasResolvableCompanionProperty =
                companionSymbol.declarationSymbols.filterIsInstance<FirPropertySymbol>().any { prop
                  ->
                  if (prop.name.asString() != propertyName) {
                    return@any false
                  }

                  // Non-const properties in companion are usable
                  if (!prop.isConst) {
                    return@any true
                  }

                  // For const vals, check if initializer is resolvable
                  val initializer = prop.fir.initializer
                  initializer != null && !hasErrorExpressionRecursive(initializer)
                }
            if (hasResolvableCompanionProperty) {
              // Found resolvable companion property - no stub needed
              return null
            }
          }
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
      sources: List<KtFile>,
      module: Module,
      missingIrClasses: Set<ClassId> = emptySet(),
  ): FirResult {
    return compileSourceFilesToAnalyzedFirViaPsi(
        sources,
        module.getModuleName(),
        module.getFriendPaths(),
        true,
        environment,
        configuration,
        missingIrClasses,
    )!!
  }

  private fun compileSourceFilesToAnalyzedFirViaPsi(
      ktFiles: List<KtFile>,
      rootModuleName: String,
      friendPaths: List<String>,
      ignoreErrors: Boolean = false,
      projectEnvironment: VfsBasedProjectEnvironment,
      configuration: CompilerConfiguration,
      missingIrClasses: Set<ClassId> = emptySet(),
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
          // Add missing IR classes detected from previous FIR run
          session.jvmAbiGenService.state.missingIrClasses.addAll(missingIrClasses)
          // Skip checkers - ABI generation only needs resolved types, and third-party
          // plugin checkers (like Litho K2) crash on unresolved references from stubs.
          val firFiles = session.buildFirFromKtFiles(sources)
          val (scopeSession, fir) = session.runResolution(firFiles)
          ModuleCompilerAnalyzedOutput(session, scopeSession, fir)
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
    val missingIrClasses = session.jvmAbiGenService.state.missingIrClasses
    return missingConstants.keys.toSet() + missingIrClasses
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
    val missingIrClasses = session.jvmAbiGenService.state.missingIrClasses

    // Generate if this class has missing constants or is a missing IR class
    val hasMissingConstants = classId in missingConstants
    val isMissingIrClass = classId in missingIrClasses

    if (!hasMissingConstants && !isMissingIrClass) {
      return null
    }

    // For missing IR classes (transitive dependencies), generate an open class
    // so it can appear in supertype chains. For missing constants, generate objects.
    val classKind = if (isMissingIrClass) ClassKind.CLASS else ClassKind.OBJECT
    val modality = if (isMissingIrClass) Modality.OPEN else Modality.FINAL

    val createdClass = createTopLevelClass(classId, JvmAbiGenPlugin, classKind)
    createdClass.replaceStatus(
        FirResolvedDeclarationStatusImpl(
                Visibilities.Public,
                modality,
                EffectiveVisibility.Public,
            )
            .apply { isStatic = !isMissingIrClass }
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
    val missingIrClasses = session.jvmAbiGenService.state.missingIrClasses

    // Return true for any package that contains classes we're generating
    return missingConstants.keys.any { classId -> classId.packageFqName == packageFqName } ||
        missingIrClasses.any { classId -> classId.packageFqName == packageFqName }
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
  val missingIrClasses: MutableSet<ClassId> = mutableSetOf()
}

val FirSession.jvmAbiGenService: JvmAbiGenService by FirSession.sessionComponentAccessor()
