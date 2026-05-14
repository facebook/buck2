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
@file:Suppress("OPT_IN_USAGE_ERROR")

package com.facebook

import org.jetbrains.kotlin.backend.common.extensions.IrPluginContext
import org.jetbrains.kotlin.ir.IrElement
import org.jetbrains.kotlin.ir.declarations.IrClass
import org.jetbrains.kotlin.ir.declarations.IrFunction
import org.jetbrains.kotlin.ir.declarations.IrModuleFragment
import org.jetbrains.kotlin.ir.declarations.IrProperty
import org.jetbrains.kotlin.ir.declarations.IrSimpleFunction
import org.jetbrains.kotlin.ir.types.IrSimpleType
import org.jetbrains.kotlin.ir.types.IrType
import org.jetbrains.kotlin.ir.types.classFqName
import org.jetbrains.kotlin.ir.types.classOrNull
import org.jetbrains.kotlin.ir.types.defaultType
import org.jetbrains.kotlin.ir.types.makeNullable
import org.jetbrains.kotlin.ir.types.typeWith
import org.jetbrains.kotlin.ir.util.hasAnnotation
import org.jetbrains.kotlin.name.ClassId
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.name.Name

/**
 * Rewrites @Composable function types in IR declarations.
 *
 * A @Composable (P1, P2, ..., Pn) -> R function type is represented in IR as FunctionN(P1, P2, ...,
 * Pn, R) with a @Composable annotation on the type.
 *
 * The Compose compiler transforms this to Function(N + 1 + changedCount)(P1, ..., Pn, Composer,
 * Int, ..., R).
 *
 * This rewriter applies that transformation to function types appearing in:
 * - Value parameter types
 * - Return types
 * - Supertype declarations
 *
 * After rewriting, the @Composable annotation is removed from the type since the synthetic
 * parameters now encode the composable nature.
 */
internal class ComposableTypeRewriter(private val pluginContext: IrPluginContext) {

  companion object {
    private val COMPOSABLE_FQ_NAME = FqName("androidx.compose.runtime.Composable")
    private val COMPOSER_CLASS_ID =
        ClassId(FqName("androidx.compose.runtime"), Name.identifier("Composer"))
    private val FUNCTION_FQ_PREFIX = "kotlin.Function"

    /** Each parameter uses 3 bits. 32 / 3 = 10 slots per Int. */
    private const val SLOTS_PER_INT = 10
  }

  private val composerType: IrType? by lazy {
    pluginContext.referenceClass(COMPOSER_CLASS_ID)?.defaultType?.makeNullable()
  }

  /** Run the type rewriting on the entire module. */
  fun transform(moduleFragment: IrModuleFragment) {
    val composerIrType = composerType ?: return
    moduleFragment.accept(TypeRewriterVisitor(composerIrType), null)
  }

  private inner class TypeRewriterVisitor(
      private val composerIrType: IrType,
  ) : IrElementVisitorVoidCompat() {

    override fun visitElement(element: IrElement) {
      element.acceptChildren(this, null)
    }

    override fun visitSimpleFunction(declaration: IrSimpleFunction) {
      rewriteFunctionSignatureTypes(declaration)
      super.visitSimpleFunction(declaration)
    }

    override fun visitProperty(declaration: IrProperty) {
      declaration.getter?.let { rewriteFunctionSignatureTypes(it) }
      declaration.setter?.let { rewriteFunctionSignatureTypes(it) }
      super.visitProperty(declaration)
    }

    override fun visitClass(declaration: IrClass) {
      // Rewrite @Composable function types in supertypes.
      declaration.superTypes = declaration.superTypes.map { rewriteTypeIfComposable(it) }
      super.visitClass(declaration)
    }

    /** Rewrite composable function types in a function's value parameters and return type. */
    private fun rewriteFunctionSignatureTypes(function: IrFunction) {
      // Rewrite parameter types.
      for (param in function.valueParameters) {
        val rewritten = rewriteTypeIfComposable(param.type)
        if (rewritten !== param.type) {
          param.type = rewritten
        }
      }

      // Rewrite return type.
      val rewrittenReturn = rewriteTypeIfComposable(function.returnType)
      if (rewrittenReturn !== function.returnType) {
        function.returnType = rewrittenReturn
      }

      // Rewrite extension receiver type.
      function.extensionReceiverParameter?.let { ext ->
        val rewritten = rewriteTypeIfComposable(ext.type)
        if (rewritten !== ext.type) {
          ext.type = rewritten
        }
      }
    }

    /**
     * If [type] is a @Composable function type, rewrite it to include Composer + $changed params.
     * Otherwise, recursively check type arguments for nested composable function types.
     */
    private fun rewriteTypeIfComposable(type: IrType): IrType {
      if (type !is IrSimpleType) return type

      // Check if this is a @Composable function type.
      if (isComposableFunctionType(type)) {
        return rewriteComposableFunctionType(type)
      }

      // Recursively rewrite type arguments (handles e.g. List<@Composable () -> Unit>).
      var anyArgChanged = false
      val rewrittenArgTypes =
          type.arguments.map { arg ->
            if (arg is org.jetbrains.kotlin.ir.types.IrTypeProjection) {
              val rewritten = rewriteTypeIfComposable(arg.type)
              if (rewritten !== arg.type) {
                anyArgChanged = true
                rewritten
              } else {
                arg.type
              }
            } else {
              pluginContext.irBuiltIns.anyNType
            }
          }

      if (anyArgChanged) {
        return type.classOrNull?.typeWith(rewrittenArgTypes) ?: type
      }

      return type
    }

    /**
     * Check if an IrSimpleType is a @Composable function type.
     *
     * A composable function type has:
     * 1. A @Composable annotation on the type
     * 2. A classifier that is kotlin.FunctionN
     */
    private fun isComposableFunctionType(type: IrSimpleType): Boolean {
      if (!type.hasAnnotation(COMPOSABLE_FQ_NAME)) return false
      val classFqn = type.classFqName?.asString() ?: return false
      return classFqn.startsWith(FUNCTION_FQ_PREFIX)
    }

    // Rewrite a @Composable function type.
    //
    // @Composable FunctionN(P1, ..., Pn, R) becomes
    // Function(N + 1 + changedCount)(P1, ..., Pn, Composer, Int, ..., R)
    //
    // The @Composable annotation is removed (the synthetic params encode composability).
    private fun rewriteComposableFunctionType(type: IrSimpleType): IrType {
      val args = type.arguments
      if (args.isEmpty()) return type

      // FunctionN<P1, ..., Pn, R>: last type arg is the return type, rest are params.
      val paramTypeArgs = args.dropLast(1)
      val returnTypeArg = args.last()

      // Number of user-visible params in the function type (no receivers in function types).
      val userParamCount = paramTypeArgs.size

      // $changed count: ceil((userParamCount + 1) / SLOTS_PER_INT), minimum 1.
      val changedCount = maxOf(1, ceilDiv(userParamCount + 1, SLOTS_PER_INT))

      // New arity: original params + Composer + changedCount Ints.
      val newArity = userParamCount + 1 + changedCount

      // Build new type arguments: [P1, ..., Pn, Composer, Int, ..., R]
      val newTypeArgs = mutableListOf<IrType>()
      for (arg in paramTypeArgs) {
        newTypeArgs.add(extractTypeFromArgument(arg))
      }
      // Add Composer? type argument.
      newTypeArgs.add(composerIrType)
      // Add Int type arguments for $changed params.
      repeat(changedCount) { newTypeArgs.add(pluginContext.irBuiltIns.intType) }
      // Add return type.
      newTypeArgs.add(extractTypeFromArgument(returnTypeArg))

      // Resolve the new FunctionN class.
      val functionClassId = ClassId(FqName("kotlin"), Name.identifier("Function$newArity"))
      val functionClass = pluginContext.referenceClass(functionClassId) ?: return type

      // Build the new type without @Composable annotation.
      return functionClass.typeWith(newTypeArgs)
    }

    private fun extractTypeFromArgument(
        arg: org.jetbrains.kotlin.ir.types.IrTypeArgument,
    ): IrType {
      return when (arg) {
        is org.jetbrains.kotlin.ir.types.IrTypeProjection -> arg.type
        else -> pluginContext.irBuiltIns.anyNType
      }
    }
  }
}

/** Integer ceiling division: ceil(a / b) for positive a, b. */
private fun ceilDiv(a: Int, b: Int): Int = (a + b - 1) / b
