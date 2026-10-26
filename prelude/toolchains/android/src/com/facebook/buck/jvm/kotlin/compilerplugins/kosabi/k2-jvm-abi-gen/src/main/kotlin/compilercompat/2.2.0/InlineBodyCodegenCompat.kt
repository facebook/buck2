/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

@file:SuppressWarnings("PackageLocationMismatch")
@file:Suppress("DEPRECATION_ERROR")
@file:OptIn(org.jetbrains.kotlin.cli.common.LegacyK2CliPipeline::class)

package com.facebook

import java.util.Collections
import java.util.IdentityHashMap
import java.util.LinkedList
import kotlin.properties.ReadWriteProperty
import kotlin.reflect.KProperty
import org.jetbrains.kotlin.backend.jvm.JvmIrCodegenFactory
import org.jetbrains.kotlin.backend.jvm.extensions.JvmIrDeclarationOrigin
import org.jetbrains.kotlin.cli.common.messages.CompilerMessageSeverity
import org.jetbrains.kotlin.cli.common.messages.MessageCollector
import org.jetbrains.kotlin.cli.jvm.compiler.legacy.pipeline.ModuleCompilerOutput
import org.jetbrains.kotlin.codegen.ClassBuilder
import org.jetbrains.kotlin.codegen.ClassBuilderFactory
import org.jetbrains.kotlin.codegen.ClassBuilderMode
import org.jetbrains.kotlin.codegen.DelegatingClassBuilder
import org.jetbrains.kotlin.codegen.OriginCollectingClassBuilderFactory
import org.jetbrains.kotlin.codegen.inline.GlobalInlineContext
import org.jetbrains.kotlin.codegen.optimization.OptimizationMethodVisitor
import org.jetbrains.kotlin.codegen.state.GenerationState
import org.jetbrains.kotlin.config.CommonConfigurationKeys
import org.jetbrains.kotlin.descriptors.CallableDescriptor
import org.jetbrains.kotlin.fir.backend.jvm.FirJvmBackendClassResolver
import org.jetbrains.kotlin.fir.backend.jvm.FirJvmBackendExtension
import org.jetbrains.kotlin.fir.backend.utils.extractFirDeclarations
import org.jetbrains.kotlin.ir.declarations.IrDeclaration
import org.jetbrains.kotlin.ir.declarations.IrDeclarationOrigin
import org.jetbrains.kotlin.ir.declarations.IrSimpleFunction
import org.jetbrains.kotlin.ir.descriptors.IrBasedDeclarationDescriptor
import org.jetbrains.kotlin.resolve.jvm.diagnostics.JvmDeclarationOrigin
import org.jetbrains.kotlin.resolve.jvm.diagnostics.JvmDeclarationOriginKind
import org.jetbrains.org.objectweb.asm.MethodVisitor

/**
 * [generateCodeFromIrCompat] that also emits the bodies of [keptInlineBodies].
 *
 * Kotlin 2.2 generates the ABI in the `KAPT3` class-builder mode, which writes a `throw null` stub
 * for every method body. The backend reads the mode once per method, right after it pushes the
 * method onto the global inline context, so a body-generating copy of the mode is installed while
 * exactly the kept methods (and the lambdas they inline) are generated. Every other read, including
 * the metadata serializer's identity checks against `KAPT3`, sees the unchanged `KAPT3` instance.
 */
internal fun generateCodeFromIrWithInlineBodiesCompat(
    input: ModuleCompilerIrBackendInputCompat,
    environment: ModuleCompilerEnvironmentCompat,
    keptInlineBodies: Set<IrSimpleFunction>,
    messageCollector: MessageCollector,
): ModuleCompilerOutputCompat {
  // The mode switch below is not thread-safe.
  input.configuration.put(CommonConfigurationKeys.PARALLEL_BACKEND_THREADS, 1)
  val builderFactory = KeptBodyOptimizingClassBuilderFactory(
      OriginCollectingClassBuilderFactory(ClassBuilderMode.KAPT3),
      keptInlineBodies,
  )
  // Mirrors the SKIP_BODIES branch of generateCodeFromIr, with only the factory replaced.
  val generationState = GenerationState(
      environment.projectEnvironment.project,
      input.irModuleFragment.descriptor,
      input.configuration,
      builderFactory,
      targetId = input.targetId,
      moduleName = input.targetId.name,
      onIndependentPartCompilationEnd = {},
      jvmBackendClassResolver = FirJvmBackendClassResolver(input.components),
      diagnosticReporter = environment.diagnosticsReporter,
  )
  builderFactory.generationState = generationState
  val tracker =
      try {
        KeptBodyTracker(keptInlineBodies, generationState, messageCollector).also {
          GlobalInlineContext::class
              .java
              .getDeclaredField("inlineCallsAndDeclarations\$delegate")
              .apply { isAccessible = true }
              .set(generationState.globalInlineContext, it.delegate)
        }
      } catch (e: Throwable) {
        if (!isKosabiCompilerInternalsFailure(e)) throw e
        reportCompilerInternalsFailure(messageCollector, e)
        return generateCodeFromIrCompat(input, environment)
      }
  val backendInput =
      JvmIrCodegenFactory.BackendInput(
          input.irModuleFragment,
          input.pluginContext.irBuiltIns,
          input.symbolTable,
          input.components.irProviders,
          input.extensions,
          FirJvmBackendExtension(
              input.components,
              input.irActualizedResult?.actualizedExpectDeclarations?.extractFirDeclarations(),
          ),
          input.pluginContext,
      )
  val codegenFactory = JvmIrCodegenFactory(input.configuration)
  codegenFactory.invokeCodegen(codegenFactory.invokeLowerings(generationState, backendInput))
  tracker.reportMissing()
  return ModuleCompilerOutput(generationState, builderFactory)
}

// GenerationState installs the bytecode optimizer only when it generates bodies, which it does not
// in the stub mode, so kept methods get the optimizer the full compilation applies.
private class KeptBodyOptimizingClassBuilderFactory(
    private val delegate: ClassBuilderFactory,
    private val keptInlineBodies: Set<IrSimpleFunction>,
) : ClassBuilderFactory {
  lateinit var generationState: GenerationState

  override fun getClassBuilderMode(): ClassBuilderMode = delegate.classBuilderMode

  override fun newClassBuilder(origin: JvmDeclarationOrigin): ClassBuilder =
      Builder(delegate.newClassBuilder(origin))

  override fun asText(builder: ClassBuilder): String = delegate.asText((builder as Builder).inner)

  override fun asBytes(builder: ClassBuilder): ByteArray =
      delegate.asBytes((builder as Builder).inner)

  private inner class Builder(val inner: ClassBuilder) : DelegatingClassBuilder() {
    override fun getDelegate(): ClassBuilder = inner

    override fun newMethod(
        origin: JvmDeclarationOrigin,
        access: Int,
        name: String,
        desc: String,
        signature: String?,
        exceptions: Array<out String>?,
    ): MethodVisitor {
      val visitor = super.newMethod(origin, access, name, desc, signature, exceptions)
      if ((origin as? JvmIrDeclarationOrigin)?.declaration !in keptInlineBodies) return visitor
      return OptimizationMethodVisitor(
          visitor,
          origin.originKind == JvmDeclarationOriginKind.INLINE_VERSION_OF_SUSPEND_FUN,
          generationState,
          access,
          name,
          desc,
          signature,
          exceptions,
      )
    }
  }
}

private class KeptBodyTracker(
    private val keptInlineBodies: Set<IrSimpleFunction>,
    private val generationState: GenerationState,
    private val messageCollector: MessageCollector,
) {
  private val emitted: MutableSet<IrSimpleFunction> = Collections.newSetFromMap(IdentityHashMap())
  private val bodiesMode =
      ClassBuilderMode::class
          .java
          .getDeclaredConstructor(
              Boolean::class.javaPrimitiveType,
              Boolean::class.javaPrimitiveType,
          )
          .apply { isAccessible = true }
          .newInstance(true, ClassBuilderMode.KAPT3.generateSourceRetentionAnnotations)
  private val modeField =
      GenerationState::class.java.getDeclaredField("classBuilderMode").apply {
        isAccessible = true
      }

  // The declarations and inline calls being generated, innermost last. FunctionCodegen pushes each
  // method's descriptor immediately before it generates the body.
  private val stack =
      object : LinkedList<Any?>() {
        override fun add(element: Any?): Boolean {
          val result = super.add(element)
          if (element is CallableDescriptor) entered(element)
          refresh()
          return result
        }

        override fun removeLast(): Any? {
          val result = super.removeLast()
          refresh()
          return result
        }
      }

  val delegate: ReadWriteProperty<Any?, LinkedList<Any?>> =
      object : ReadWriteProperty<Any?, LinkedList<Any?>> {
        override fun getValue(thisRef: Any?, property: KProperty<*>): LinkedList<Any?> = stack

        override fun setValue(thisRef: Any?, property: KProperty<*>, value: LinkedList<Any?>) {}
      }

  private fun keptOwner(descriptor: CallableDescriptor): IrSimpleFunction? {
    var current = (descriptor as? IrBasedDeclarationDescriptor<*>)?.owner as? IrDeclaration
    while (current != null) {
      if (current is IrSimpleFunction) {
        if (current in keptInlineBodies) return current
        liftedLambdaOwner(current)?.let {
          return it
        }
      }
      current = current.parent as? IrDeclaration
    }
    return null
  }

  // Lowering lifts each inlined lambda into a sibling `<function>$lambda$<n>` method. Only kept
  // bodies still contain lambdas at this point.
  private fun liftedLambdaOwner(function: IrSimpleFunction): IrSimpleFunction? {
    if (function.origin != IrDeclarationOrigin.INLINE_LAMBDA) return null
    val name = function.name.asString()
    return keptInlineBodies.firstOrNull {
      it.parent == function.parent && name.startsWith(it.name.asString() + "\$lambda\$")
    }
  }

  private fun entered(descriptor: CallableDescriptor) {
    val owner = keptOwner(descriptor)
    if (owner != null) {
      if ((descriptor as IrBasedDeclarationDescriptor<*>).owner === owner) emitted += owner
      return
    }
    // A method generated inside a kept body would be written as a stub but run as real code.
    val enclosing = stack.filterIsInstance<CallableDescriptor>().firstNotNullOfOrNull(::keptOwner)
    if (enclosing != null) {
      fail(enclosing.name.asString(), "generating it also generated stubbed method $descriptor")
    }
  }

  private fun refresh() {
    val top = stack.lastOrNull { it is CallableDescriptor } as CallableDescriptor?
    val mode = if (top != null && keptOwner(top) != null) bodiesMode else ClassBuilderMode.KAPT3
    if (generationState.classBuilderMode !== mode) modeField.set(generationState, mode)
  }

  fun reportMissing() {
    for (function in keptInlineBodies) {
      if (function !in emitted) fail(function.name.asString(), "its method was not generated")
    }
  }

  private fun fail(name: String, reason: String) {
    messageCollector.report(
        CompilerMessageSeverity.ERROR,
        "Kosabi inline bodies: could not emit the body of inline function `$name`: $reason. " +
            "Its callers would inline an empty body.",
    )
  }
}
