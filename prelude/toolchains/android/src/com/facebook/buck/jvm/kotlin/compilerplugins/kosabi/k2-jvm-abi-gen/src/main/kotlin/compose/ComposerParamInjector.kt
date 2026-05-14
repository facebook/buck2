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
import org.jetbrains.kotlin.ir.declarations.IrDeclarationOrigin
import org.jetbrains.kotlin.ir.declarations.IrModuleFragment
import org.jetbrains.kotlin.ir.declarations.IrProperty
import org.jetbrains.kotlin.ir.declarations.IrSimpleFunction
import org.jetbrains.kotlin.ir.declarations.IrValueParameter
import org.jetbrains.kotlin.ir.symbols.UnsafeDuringIrConstructionAPI
import org.jetbrains.kotlin.ir.types.IrType
import org.jetbrains.kotlin.ir.types.defaultType
import org.jetbrains.kotlin.ir.types.makeNullable
import org.jetbrains.kotlin.ir.util.hasAnnotation
import org.jetbrains.kotlin.name.ClassId
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.name.Name

/**
 * Injects Compose synthetic parameters ($composer, $changed, $default) into @Composable functions.
 *
 * For every @Composable function (excluding constructors and expect functions), this adds:
 * - `$composer: Composer?` — the Composer instance threaded through composable calls
 * - `$changed: Int` (1 or more) — bitmask tracking parameter change state
 * - `$default: Int` (0 or more) — bitmask for default parameter values, only when defaults exist
 *
 * Parameter counting follows the Compose compiler spec:
 * - changedParamCount = max(1, ceil((realValueParams + thisParams + 1) / SLOTS_PER_INT)) where +1
 *   accounts for the force bit in slot 0
 * - defaultParamCount = ceil(valueParams / BITS_PER_DEFAULT_INT), only if any param has a default
 * - thisParams = count of dispatch receiver + extension receiver (context receivers are value
 *   params)
 */
@OptIn(UnsafeDuringIrConstructionAPI::class)
internal class ComposerParamInjector(private val pluginContext: IrPluginContext) {

  companion object {
    val COMPOSABLE_FQ_NAME = FqName("androidx.compose.runtime.Composable")
    private val COMPOSER_CLASS_ID =
        ClassId(FqName("androidx.compose.runtime"), Name.identifier("Composer"))

    /** Each parameter uses 3 bits in the $changed bitmask. 32 / 3 = 10 slots per Int. */
    private const val SLOTS_PER_INT = 10

    /** Each $default Int can track 31 parameters (bit 31 is sign bit, unusable). */
    private const val BITS_PER_DEFAULT_INT = 31
  }

  /** The Composer type, resolved lazily. Nullable if Compose runtime is not on classpath. */
  private val composerType: IrType? by lazy {
    pluginContext.referenceClass(COMPOSER_CLASS_ID)?.defaultType?.makeNullable()
  }

  /** Run the transform on the entire module. */
  fun transform(moduleFragment: IrModuleFragment) {
    val composerIrType = composerType ?: return // Compose runtime not on classpath; skip.
    moduleFragment.accept(
        ComposerParamVisitor(pluginContext, composerIrType),
        null,
    )
  }

  private inner class ComposerParamVisitor(
      private val pluginContext: IrPluginContext,
      private val composerIrType: IrType,
  ) : IrElementVisitorVoidCompat() {

    override fun visitElement(element: IrElement) {
      element.acceptChildren(this, null)
    }

    override fun visitSimpleFunction(declaration: IrSimpleFunction) {
      if (shouldTransform(declaration)) {
        injectParams(declaration)
      }
      // Continue visiting nested declarations (local functions inside composables).
      super.visitSimpleFunction(declaration)
    }

    override fun visitClass(declaration: IrClass) {
      // Visit class members.
      super.visitClass(declaration)
    }

    override fun visitProperty(declaration: IrProperty) {
      // Property getters can be @Composable.
      declaration.getter?.let { getter ->
        if (shouldTransform(getter)) {
          injectParams(getter)
        }
      }
      super.visitProperty(declaration)
    }

    /**
     * Determine if a function should be transformed.
     *
     * Transform if:
     * - Has @Composable annotation
     * - Is not a constructor
     * - Is not an expect declaration
     */
    private fun shouldTransform(function: IrSimpleFunction): Boolean {
      if (!function.hasAnnotation(COMPOSABLE_FQ_NAME)) return false
      if (function.isExpect) return false
      return true
    }

    /**
     * Inject $composer, $changed, and $default parameters into a @Composable function.
     *
     * Parameters are appended after all existing value parameters.
     */
    private fun injectParams(function: IrSimpleFunction) {
      val existingValueParams = function.valueParameters

      // Count "this" params: dispatch receiver + extension receiver.
      // These contribute to $changed slot allocation but are not value parameters.
      val thisParamCount =
          (if (function.dispatchReceiverParameter != null) 1 else 0) +
              (if (function.extensionReceiverParameter != null) 1 else 0)

      val realValueParamCount = existingValueParams.size
      val totalSlottedParams = realValueParamCount + thisParamCount

      // $changed count: ceil((totalSlottedParams + 1) / SLOTS_PER_INT), minimum 1.
      // The +1 accounts for the force bit in slot 0.
      val changedCount = maxOf(1, ceilDiv(totalSlottedParams + 1, SLOTS_PER_INT))

      // $default count: only present if any parameter has a default value.
      val hasDefaults = existingValueParams.any { it.defaultValue != null }
      val defaultCount = if (hasDefaults) ceilDiv(realValueParamCount, BITS_PER_DEFAULT_INT) else 0

      // Build the new parameter list: existing + $composer + $changed[N] + $default[N]
      val newParams = mutableListOf<IrValueParameter>()
      newParams.addAll(existingValueParams)

      var paramIndex = existingValueParams.size

      // Add $composer: Composer?
      newParams.add(
          pluginContext.irFactory
              .createValueParameter(
                  startOffset = -1,
                  endOffset = -1,
                  origin = IrDeclarationOrigin.DEFINED,
                  name = Name.identifier("\$composer"),
                  type = composerIrType,
                  isAssignable = false,
                  symbol = org.jetbrains.kotlin.ir.symbols.impl.IrValueParameterSymbolImpl(),
                  index = paramIndex,
                  varargElementType = null,
                  isCrossinline = false,
                  isNoinline = false,
                  isHidden = false,
              )
              .also { it.parent = function },
      )
      paramIndex++

      // Add $changed, $changed1, $changed2, ...
      for (i in 0 until changedCount) {
        val name = if (i == 0) "\$changed" else "\$changed$i"
        newParams.add(
            pluginContext.irFactory
                .createValueParameter(
                    startOffset = -1,
                    endOffset = -1,
                    origin = IrDeclarationOrigin.DEFINED,
                    name = Name.identifier(name),
                    type = pluginContext.irBuiltIns.intType,
                    isAssignable = false,
                    symbol = org.jetbrains.kotlin.ir.symbols.impl.IrValueParameterSymbolImpl(),
                    index = paramIndex,
                    varargElementType = null,
                    isCrossinline = false,
                    isNoinline = false,
                    isHidden = false,
                )
                .also { it.parent = function },
        )
        paramIndex++
      }

      // Add $default, $default1, $default2, ...
      for (i in 0 until defaultCount) {
        val name = if (i == 0) "\$default" else "\$default$i"
        newParams.add(
            pluginContext.irFactory
                .createValueParameter(
                    startOffset = -1,
                    endOffset = -1,
                    origin = IrDeclarationOrigin.DEFINED,
                    name = Name.identifier(name),
                    type = pluginContext.irBuiltIns.intType,
                    isAssignable = false,
                    symbol = org.jetbrains.kotlin.ir.symbols.impl.IrValueParameterSymbolImpl(),
                    index = paramIndex,
                    varargElementType = null,
                    isCrossinline = false,
                    isNoinline = false,
                    isHidden = false,
                )
                .also { it.parent = function },
        )
        paramIndex++
      }

      function.valueParameters = newParams
    }
  }
}

/** Integer ceiling division: ceil(a / b) for positive a, b. */
private fun ceilDiv(a: Int, b: Int): Int = (a + b - 1) / b
