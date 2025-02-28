/*
 * Portions Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

/*
 * Copyright 2010-2018 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package com.facebook

import com.facebook.asm.AbiClassBuilder
import com.facebook.asm.FilterInnerClassesVisitor
import com.facebook.asm.InnerClassesCollectingVisitor
import com.facebook.stub.MixedCompilationUtil.convertGeneratedJavaSourcesToKotlin
import java.io.File
import kotlinx.coroutines.TimeoutCancellationException
import kotlinx.coroutines.launch
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.withTimeout
import main.kotlin.stub.isGeneratedFromStub
import main.kotlin.stub.isStub
import org.jetbrains.kotlin.analyzer.AnalysisResult
import org.jetbrains.kotlin.cli.common.CLIConfigurationKeys
import org.jetbrains.kotlin.cli.common.messages.CompilerMessageSeverity
import org.jetbrains.kotlin.cli.common.messages.MessageCollector
import org.jetbrains.kotlin.cli.common.messages.MessageRenderer
import org.jetbrains.kotlin.cli.common.messages.OutputMessageUtil
import org.jetbrains.kotlin.cli.common.messages.PrintingMessageCollector
import org.jetbrains.kotlin.codegen.ClassBuilder
import org.jetbrains.kotlin.codegen.ClassBuilderFactory
import org.jetbrains.kotlin.codegen.ClassBuilderMode
import org.jetbrains.kotlin.codegen.KotlinCodegenFacade
import org.jetbrains.kotlin.codegen.state.GenerationState
import org.jetbrains.kotlin.com.intellij.openapi.project.Project
import org.jetbrains.kotlin.com.intellij.openapi.util.io.FileUtil
import org.jetbrains.kotlin.compilerRunner.OutputItemsCollector
import org.jetbrains.kotlin.config.CommonConfigurationKeys
import org.jetbrains.kotlin.config.CompilerConfiguration
import org.jetbrains.kotlin.config.JVMConfigurationKeys
import org.jetbrains.kotlin.container.ComponentProvider
import org.jetbrains.kotlin.context.ProjectContext
import org.jetbrains.kotlin.descriptors.ModuleDescriptor
import org.jetbrains.kotlin.incremental.isClassFile
import org.jetbrains.kotlin.load.kotlin.FileBasedKotlinClass
import org.jetbrains.kotlin.load.kotlin.header.KotlinClassHeader
import org.jetbrains.kotlin.metadata.ProtoBuf
import org.jetbrains.kotlin.metadata.deserialization.Flags
import org.jetbrains.kotlin.metadata.jvm.deserialization.JvmMetadataVersion
import org.jetbrains.kotlin.metadata.jvm.deserialization.JvmProtoBufUtil
import org.jetbrains.kotlin.modules.TargetId
import org.jetbrains.kotlin.name.ClassId
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.kotlin.resolve.BindingTrace
import org.jetbrains.kotlin.resolve.extensions.AnalysisHandlerExtension
import org.jetbrains.kotlin.resolve.jvm.JvmClassName
import org.jetbrains.kotlin.resolve.jvm.diagnostics.JvmDeclarationOrigin
import org.jetbrains.org.objectweb.asm.ClassReader
import org.jetbrains.org.objectweb.asm.ClassVisitor
import org.jetbrains.org.objectweb.asm.ClassWriter
import org.jetbrains.org.objectweb.asm.Opcodes

@OptIn(org.jetbrains.kotlin.compiler.plugin.ExperimentalCompilerApi::class)
@SuppressWarnings("PackageLocationMismatch")
class JvmAbiAnalysisHandlerExtension(compilerConfiguration: CompilerConfiguration) :
    AnalysisHandlerExtension {
  private val compilerConfiguration: CompilerConfiguration =
      compilerConfiguration.copy().apply { put(JVMConfigurationKeys.IR, false) }
  private val isMixedCompilationEnabled: Boolean =
      compilerConfiguration.get(JvmAbiConfigurationKeys.ENABLE_MIXED_COMPILATION, false)
  private var hasDoneDoAnalysis = false
  private var hasGeneratedCode = false
  private var javaKStubs: List<File> = emptyList()

  override fun doAnalysis(
      project: Project,
      module: ModuleDescriptor,
      projectContext: ProjectContext,
      files: Collection<KtFile>,
      bindingTrace: BindingTrace,
      componentProvider: ComponentProvider
  ): AnalysisResult? {
    if (isMixedCompilationEnabled && !hasGeneratedCode) {
      val java2KotlinSourceFiles =
          convertGeneratedJavaSourcesToKotlin(project, compilerConfiguration, files)
      hasGeneratedCode = true
      javaKStubs = java2KotlinSourceFiles.map { File(it.virtualFilePath) }
      if (javaKStubs.isNotEmpty()) {
        // Register KStubs file to be deleted to prevent unexpected crashes intercept in the process
        javaKStubs.forEach { it.deleteOnExit() }
        return AnalysisResult.RetryWithAdditionalRoots(
            bindingTrace.bindingContext,
            module,
            emptyList<File>(),
            javaKStubs,
        )
      }
    }
    hasDoneDoAnalysis = true
    return null
  }

  override fun analysisCompleted(
      project: Project,
      module: ModuleDescriptor,
      bindingTrace: BindingTrace,
      files: Collection<KtFile>
  ): AnalysisResult? {
    if (!hasDoneDoAnalysis) {
      return null
    }

    val bindingContext = bindingTrace.bindingContext
    // Ignoring all diagnostics, but should consider turning this check on again
    // We want to try abi-generation even if we get a compilation error at some point.
    // We should not get any compilation errors for missing class symbols; however we might want to
    // process ABI with some missing methods.
    // if (bindingContext.diagnostics.any { it.severity == Severity.ERROR }) return null

    val targetId =
        TargetId(
            name =
                compilerConfiguration[CommonConfigurationKeys.MODULE_NAME]
                    ?: module.name.asString(),
            type = "java-production")

    val generationState =
        GenerationState.Builder(
                project, AbiBinaries, module, bindingContext, files.toList(), compilerConfiguration)
            .targetId(targetId)
            .build()

    // Kosabi will read KSP generated Java code and generate KStubs to fill in the Abi info.
    // However, we don't want to keep them in the system after Abi generation done.
    // We will only move the [javaKStubs] to debug directory for testing and debugging purpose.
    fun cleanupGeneratedJavaKStubs() {
      if (javaKStubs.isEmpty()) {
        return
      }

      val debugKStubOutputDir: File? =
          compilerConfiguration.get(JvmAbiConfigurationKeys.DEBUG_JAVA_KSTUB_OUTPUT_PATH)?.let {
            File(it)
          }
      // The [debugKStubOutputDir] will be temporary directory and the deletion will happen in the
      // unit test @AfterEach
      if (debugKStubOutputDir != null) {
        javaKStubs.forEach {
          val newFile = File(debugKStubOutputDir, it.name)
          it.renameTo(newFile)
        }
      } else {
        @Suppress("UseOfRunBlocking")
        runBlocking {
          try {
            withTimeout(1000L) { javaKStubs.forEach { launch { it.delete() } } }
          } catch (ex: TimeoutCancellationException) {
            val messageCollector =
                compilerConfiguration.get(CLIConfigurationKeys.MESSAGE_COLLECTOR_KEY)
                    ?: PrintingMessageCollector(System.err, MessageRenderer.PLAIN_FULL_PATHS, false)
            ex.message?.let { messageCollector.report(CompilerMessageSeverity.WARNING, it) }
          }
        }
      }
    }

    // Handle unexpected state with COMPILATION_ERROR to give extra details for developers.
    // Invalid stubs caused IllegalStateException thrown by
    // [KotlinCodegenFacade.compileCorrectFiles]
    fun endCompilationWithCompilationErrorException(
        collector: MessageCollector,
        errorMessage: String
    ): AnalysisResult {
      val targetNamePointer = targetId.name.lastIndexOf('.')
      var additionalMessage: String = ""
      // If targetId is able to reassemble into target path, then we will add instruction to
      // run Kosabi error analyse script to the target to determine next step.
      if (targetNamePointer > 0) {
        val targetPath =
            "//${targetId.name
                .replace('.', '/')
                .replaceRange(targetNamePointer, targetNamePointer + 1, ":")}"
        val analyzeKosabiErrorCommand = "./xplat/kotlin/kosabi/analyze_error.py $targetPath"
        additionalMessage =
            """
          TO RESOLVE, please run command below:
          | ```
          | $analyzeKosabiErrorCommand
          | ```
        """
                .trimIndent()
      }

      // messageCollector.report(...EXCEPTION...) will lead to COMPILATION_ERROR exit code.
      collector.report(
          CompilerMessageSeverity.EXCEPTION,
          OutputMessageUtil.renderException(
              RuntimeException(
                  """
                | Unexpected error in Kotlin Kosabi compilation. $additionalMessage
                | For further assistance, please reach out https://fburl.com/kotlinfoundationgroup.
                | =======================================================================
                | $errorMessage
              """
                      .trimMargin())))
      return AnalysisResult.compilationError(bindingContext)
    }

    try {
      KotlinCodegenFacade.compileCorrectFiles(generationState)
    } catch (err: Exception) {
      // Clean up if any unexpected exception happens.
      cleanupGeneratedJavaKStubs()
      val messageCollector =
          compilerConfiguration.get(CLIConfigurationKeys.MESSAGE_COLLECTOR_KEY)
              ?: PrintingMessageCollector(System.err, MessageRenderer.PLAIN_FULL_PATHS, false)
      return endCompilationWithCompilationErrorException(
          messageCollector, "KotlinCodegenFacade compilation error: $err")
    }

    val outputDir = compilerConfiguration.get(JVMConfigurationKeys.OUTPUT_DIRECTORY)!!
    val outputs = ArrayList<AbiOutput>()

    val stubs = files.filter { it.isStub() }

    generationState.factory
        .asList()
        .filter { outputFile -> !outputFile.isGeneratedFromStub(stubs) }
        .forEach {
          val file = File(outputDir, it.relativePath)
          outputs.add(AbiOutput(file, it.sourceFiles, it.asByteArray()))
        }

    // Clean up generated KStubs after abi generation.
    cleanupGeneratedJavaKStubs()

    // private/local/synthetic class removal is temporarily turned off, because the implementation
    // was not correct: it was not taking into account that private/local classes could be used
    // from inline functions
    // todo: implement correct removal
    // removeUnneededClasses(outputs)

    val messageCollector =
        compilerConfiguration.get(CLIConfigurationKeys.MESSAGE_COLLECTOR_KEY)
            ?: PrintingMessageCollector(System.err, MessageRenderer.PLAIN_FULL_PATHS, false)
    val reportOutputFiles =
        generationState.configuration.getBoolean(CommonConfigurationKeys.REPORT_OUTPUT_FILES)
    val outputItemsCollector =
        OutputItemsCollector { sourceFiles, outputFile ->
              messageCollector.report(
                  CompilerMessageSeverity.OUTPUT,
                  OutputMessageUtil.formatOutputMessage(sourceFiles, outputFile))
            }
            .takeIf { reportOutputFiles }
    outputs.forEach { it.flush(outputItemsCollector) }

    // For class abi use, we do not want early termination.
    // For Kosabi, at this point ABI should be written.
    // If it is not true, it's likely Compilation is triggered 2nd time, possibly with Additional
    // Roots (as in abi-stubs-gen plugin)
    if (compilerConfiguration.get(JvmAbiConfigurationKeys.EARLY_TERMINATION, false)) {
      // Changing to InternalError propagation for simplicity for now
      // In some cases returning AnalysisResult.internalError() in not enough to exit compilation
      // with an Internal Error (2) code.
      // Throwing declaration always leads to an InternalError as we need
      throw RuntimeException("Terminating compilation. We're done with ABI.")
    }

    return null
  }

  /** Removes private or local classes from outputs */
  // todo: fix usage (see analysisCompleted)
  @Suppress("unused")
  private fun removeUnneededClasses(outputs: Iterable<AbiOutput>) {
    // maps internal names of classes: class -> inner classes
    val innerClasses = HashMap<String, Collection<String>>()
    val internalNameToFile = HashMap<String, File>()

    for (output in outputs) {
      if (!output.file.isClassFile()) {
        continue
      }

      val visitor = InnerClassesCollectingVisitor()
      output.accept(visitor)
      val outputInternalName = visitor.ownInternalName
      internalNameToFile[outputInternalName] = output.file
      innerClasses[outputInternalName] = visitor.innerClasses
    }

    // internal names of removed files
    val classesToRemoveQueue = ArrayDeque<String>()
    for (output in outputs) {
      if (!output.file.isClassFile()) {
        continue
      }

      val classData = output.classData() ?: continue
      val header = classData.classHeader
      val isNeededForAbi =
          when (header.kind) {
            KotlinClassHeader.Kind.CLASS -> {
              val (_, classProto) =
                  JvmProtoBufUtil.readClassDataFrom(header.data!!, header.strings!!)
              val visibility = Flags.VISIBILITY.get(classProto.flags)
              visibility != ProtoBuf.Visibility.PRIVATE && visibility != ProtoBuf.Visibility.LOCAL
            }
            KotlinClassHeader.Kind.SYNTHETIC_CLASS -> false
            else -> true
          }

      if (!isNeededForAbi) {
        val jvmClassName = JvmClassName.byClassId(classData.classId)
        classesToRemoveQueue.add(jvmClassName.internalName)
      }
    }

    // we can remove inner classes of removed classes
    val classesToRemove = HashSet<String>()
    classesToRemove.addAll(classesToRemoveQueue)
    while (classesToRemoveQueue.isNotEmpty()) {
      val classToRemove = classesToRemoveQueue.removeFirst()
      innerClasses[classToRemove]?.forEach {
        if (classesToRemove.add(it)) {
          classesToRemoveQueue.add(it)
        }
      }
    }

    val classFilesToRemove = classesToRemove.mapTo(HashSet()) { internalNameToFile[it] }
    for (output in outputs) {
      if (!output.file.isClassFile()) {
        continue
      }

      if (output.file in classFilesToRemove) {
        output.delete()
      } else {
        output.transform { writer ->
          FilterInnerClassesVisitor(classesToRemove, Opcodes.API_VERSION, writer)
        }
      }
    }
  }

  private object AbiBinaries : ClassBuilderFactory {
    override fun getClassBuilderMode(): ClassBuilderMode = ClassBuilderMode.ABI

    override fun newClassBuilder(origin: JvmDeclarationOrigin): ClassBuilder =
        AbiClassBuilder(ClassWriter(0))

    override fun asText(builder: ClassBuilder): String =
        throw UnsupportedOperationException("AbiBinaries generator asked for text")

    override fun asBytes(builder: ClassBuilder): ByteArray {
      val visitor = builder.visitor as ClassWriter
      return visitor.toByteArray()
    }

    override fun close() = Unit
  }

  private data class ClassData(
      val classId: ClassId,
      val classVersion: Int,
      val classHeader: KotlinClassHeader
  )

  private class AbiOutput(
      val file: File,
      val sources: List<File>,
      // null bytes means that file should not be written
      private var bytes: ByteArray?
  ) {
    fun classData(): ClassData? =
        when {
          bytes == null -> null
          !file.isClassFile() -> null
          else ->
              FileBasedKotlinClass.create(bytes!!, JvmMetadataVersion.INSTANCE) {
                  classId,
                  classVersion,
                  classHeader,
                  _ ->
                ClassData(classId, classVersion, classHeader)
              }
        }

    fun delete() {
      bytes = null
    }

    fun transform(fn: (writer: ClassWriter) -> ClassVisitor) {
      val bytes = bytes ?: return
      val cr = ClassReader(bytes)
      val cw = ClassWriter(0)
      val visitor = fn(cw)
      cr.accept(visitor, 0)
      this.bytes = cw.toByteArray()
    }

    fun accept(visitor: ClassVisitor) {
      val bytes = bytes ?: return
      val cr = ClassReader(bytes)
      cr.accept(visitor, 0)
    }

    fun flush(outputItemsCollector: OutputItemsCollector?) {
      val bytes = bytes ?: return
      FileUtil.writeToFile(file, bytes)
      outputItemsCollector?.add(sources, file)
    }
  }
}
