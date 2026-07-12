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
import org.jetbrains.kotlin.backend.common.lower.createIrBuilder
import org.jetbrains.kotlin.descriptors.ClassKind
import org.jetbrains.kotlin.descriptors.DescriptorVisibilities
import org.jetbrains.kotlin.ir.IrElement
import org.jetbrains.kotlin.ir.builders.irCallConstructor
import org.jetbrains.kotlin.ir.declarations.IrClass
import org.jetbrains.kotlin.ir.declarations.IrDeclaration
import org.jetbrains.kotlin.ir.declarations.IrDeclarationOrigin
import org.jetbrains.kotlin.ir.declarations.IrField
import org.jetbrains.kotlin.ir.declarations.IrModuleFragment
import org.jetbrains.kotlin.ir.declarations.IrProperty
import org.jetbrains.kotlin.ir.expressions.impl.IrConstImpl
import org.jetbrains.kotlin.ir.symbols.IrClassSymbol
import org.jetbrains.kotlin.ir.symbols.UnsafeDuringIrConstructionAPI
import org.jetbrains.kotlin.ir.types.IrSimpleType
import org.jetbrains.kotlin.ir.types.IrType
import org.jetbrains.kotlin.ir.types.classFqName
import org.jetbrains.kotlin.ir.types.classOrNull
import org.jetbrains.kotlin.ir.types.isBoolean
import org.jetbrains.kotlin.ir.types.isByte
import org.jetbrains.kotlin.ir.types.isChar
import org.jetbrains.kotlin.ir.types.isDouble
import org.jetbrains.kotlin.ir.types.isFloat
import org.jetbrains.kotlin.ir.types.isInt
import org.jetbrains.kotlin.ir.types.isLong
import org.jetbrains.kotlin.ir.types.isNullable
import org.jetbrains.kotlin.ir.types.isShort
import org.jetbrains.kotlin.ir.types.isString
import org.jetbrains.kotlin.ir.types.isUnit
import org.jetbrains.kotlin.ir.types.makeNotNull
import org.jetbrains.kotlin.ir.util.constructors
import org.jetbrains.kotlin.ir.util.isEnumClass
import org.jetbrains.kotlin.ir.util.isEnumEntry
import org.jetbrains.kotlin.ir.util.isFileClass
import org.jetbrains.kotlin.ir.util.isInterface
import org.jetbrains.kotlin.name.ClassId
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.name.Name

/**
 * Adds $stable static field and @StabilityInferred annotation to eligible classes.
 *
 * The Compose compiler adds $stable and @StabilityInferred to every eligible class so downstream
 * modules can determine stability without re-analyzing fields. This emulation replicates both
 * artifacts for source-only ABI.
 *
 * Eligible classes: public/internal, non-enum, non-interface, non-annotation, non-anonymous,
 * non-expect, non-inner, non-file-class, non-companion, non-inline/value.
 *
 * Stability rules (simplified for ABI emulation):
 * - val properties of stable types: stable
 * - var properties (non-delegated): unstable
 * - Primitives, String, Unit, enums, function types: stable
 * - Type parameters: encoded in the bitmask
 * - Everything else: conservatively unstable (0)
 */
@OptIn(UnsafeDuringIrConstructionAPI::class)
internal class ClassStabilityTransformer(private val pluginContext: IrPluginContext) {

  companion object {
    private val COMPOSER_CLASS_ID =
        ClassId(FqName("androidx.compose.runtime"), Name.identifier("Composer"))
    private val STABILITY_INFERRED_CLASS_ID =
        ClassId(FqName("androidx.compose.runtime.internal"), Name.identifier("StabilityInferred"))

    // Known stable FQNs beyond primitives/String/Unit
    private val KNOWN_STABLE_FQNS = setOf(
        "kotlin.Pair",
        "kotlin.Triple",
        "kotlin.Result",
        "kotlin.ranges.IntRange",
        "kotlin.ranges.LongRange",
        "kotlin.ranges.CharRange",
        "kotlin.coroutines.CoroutineContext.Key",
        "kotlinx.collections.immutable.ImmutableList",
        "kotlinx.collections.immutable.ImmutableSet",
        "kotlinx.collections.immutable.ImmutableMap",
        "kotlinx.collections.immutable.ImmutableCollection",
        "kotlinx.collections.immutable.PersistentList",
        "kotlinx.collections.immutable.PersistentSet",
        "kotlinx.collections.immutable.PersistentMap",
        "dagger.Lazy",
    )
  }

  // Classes currently being analyzed (cycle detection)
  private val analyzing = mutableSetOf<String>()

  // Lazily resolved — null if compose-runtime is not on the classpath
  private var stabilityInferredClass: IrClassSymbol? = null

  fun transform(moduleFragment: IrModuleFragment) {
    // No-op when Compose runtime is not on the classpath.
    pluginContext.referenceClass(COMPOSER_CLASS_ID) ?: return
    stabilityInferredClass = pluginContext.referenceClass(STABILITY_INFERRED_CLASS_ID)
    moduleFragment.accept(StabilityVisitor(), null)
  }

  private inner class StabilityVisitor : IrElementVisitorVoidCompat() {

    override fun visitElement(element: IrElement) {
      element.acceptChildren(this, null)
    }

    override fun visitClass(declaration: IrClass) {
      if (shouldTransform(declaration)) {
        addStabilityField(declaration)
      }
      // Visit nested classes
      super.visitClass(declaration)
    }
  }

  private fun shouldTransform(irClass: IrClass): Boolean {
    // Skip enum classes and enum entries
    if (irClass.isEnumClass || irClass.isEnumEntry) return false
    // Skip interfaces
    if (irClass.isInterface) return false
    // Skip annotation classes
    if (irClass.kind == ClassKind.ANNOTATION_CLASS) return false
    // Skip anonymous objects
    if (irClass.kind == ClassKind.OBJECT && irClass.name.isSpecial) return false
    // Skip expect declarations
    if (irClass.isExpect) return false
    // Skip inner classes
    if (irClass.isInner) return false
    // Skip file classes (top-level function holders)
    if (irClass.isFileClass) return false
    // Skip companion objects
    if (irClass.isCompanion) return false
    // Skip inline/value classes
    if (irClass.isValue) return false
    // Only process public/internal classes
    val vis = irClass.visibility
    if (
        vis != DescriptorVisibilities.PUBLIC &&
            vis != DescriptorVisibilities.INTERNAL &&
            vis != DescriptorVisibilities.PROTECTED
    ) {
      return false
    }
    return true
  }

  private fun addStabilityField(irClass: IrClass) {
    val stableValue = computeStabilityValue(irClass)

    // Add static final $stable: Int field
    val stableField =
        pluginContext.irFactory.createField(
            startOffset = -1,
            endOffset = -1,
            origin = IrDeclarationOrigin.DEFINED,
            name = Name.identifier("\$stable"),
            type = pluginContext.irBuiltIns.intType,
            visibility = DescriptorVisibilities.PUBLIC,
            symbol = org.jetbrains.kotlin.ir.symbols.impl.IrFieldSymbolImpl(),
            isFinal = true,
            isExternal = false,
            isStatic = true,
        )
    stableField.parent = irClass
    val constExpr = IrConstImpl.int(-1, -1, pluginContext.irBuiltIns.intType, stableValue)
    stableField.initializer = pluginContext.irFactory.createExpressionBody(-1, -1, constExpr)
    irClass.declarations.add(stableField)

    addStabilityInferredAnnotation(irClass, stableValue)
  }

  private fun addStabilityInferredAnnotation(irClass: IrClass, parametersValue: Int) {
    val annotationClass = stabilityInferredClass ?: return
    val constructor = annotationClass.constructors.singleOrNull() ?: return
    val annotation =
        pluginContext.irBuiltIns.createIrBuilder(irClass.symbol).run {
          irCallConstructor(constructor, emptyList()).apply {
            putValueArgument(
                0,
                IrConstImpl.int(-1, -1, pluginContext.irBuiltIns.intType, parametersValue),
            )
          }
        }
    irClass.annotations += annotation
  }

  // Compute the $stable field value.
  // 0 = all stable, non-zero encodes which type parameters affect stability.
  // For simplicity in ABI emulation, we compute based on property analysis.
  private fun computeStabilityValue(irClass: IrClass): Int {
    val classFqn = irClass.classFqName()
    if (classFqn != null && analyzing.contains(classFqn)) {
      return 0 // Cycle — treat as stable to avoid infinite recursion
    }
    if (classFqn != null) analyzing.add(classFqn)

    var result = 0
    for (declaration in irClass.declarations) {
      when (declaration) {
        is IrProperty -> {
          // var (non-delegated) → unstable
          if (declaration.isVar && declaration.isDelegated != true) {
            if (classFqn != null) analyzing.remove(classFqn)
            return 0 // Unstable class gets $stable = 0 (correct — means "evaluate at runtime")
          }
          // Check backing field type for type parameter dependency
          val backingField = declaration.backingField
          if (backingField != null) {
            result = result or typeParamBits(backingField.type, irClass)
          }
        }
        is IrField -> {
          if (!declaration.isStatic) {
            result = result or typeParamBits(declaration.type, irClass)
          }
        }
        else -> {}
      }
    }

    if (classFqn != null) analyzing.remove(classFqn)
    return result
  }

  // Determine which type parameter bits are affected by a given type.
  // Returns a bitmask where bit N is set if type parameter N of the class appears in the type.
  private fun typeParamBits(type: IrType, irClass: IrClass): Int {
    if (irClass.typeParameters.isEmpty()) return 0

    // If the type is already known stable, no type param dependency
    if (isKnownStableType(type)) return 0

    // Check if the type IS a type parameter of this class
    if (type is IrSimpleType) {
      val classifier = type.classifier
      for ((index, typeParam) in irClass.typeParameters.withIndex()) {
        if (classifier == typeParam.symbol) {
          return 1 shl index
        }
      }

      // Recurse into type arguments
      var bits = 0
      for (arg in type.arguments) {
        if (arg is org.jetbrains.kotlin.ir.types.IrTypeProjection) {
          bits = bits or typeParamBits(arg.type, irClass)
        }
      }
      return bits
    }
    return 0
  }

  // Check if a type is known stable (primitives, String, enums, function types, etc.)
  private fun isKnownStableType(type: IrType): Boolean {
    // Handle nullable — strip and check inner
    if (type.isNullable()) {
      return isKnownStableType(type.makeNotNull())
    }

    // Primitives
    if (
        type.isInt() ||
            type.isLong() ||
            type.isFloat() ||
            type.isDouble() ||
            type.isBoolean() ||
            type.isByte() ||
            type.isShort() ||
            type.isChar()
    ) {
      return true
    }
    // String, Unit
    if (type.isString() || type.isUnit()) return true

    val fqn = type.classFqName?.asString() ?: return false

    // Function types
    if (fqn.startsWith("kotlin.Function") || fqn.startsWith("kotlin.reflect.KFunction")) {
      return true
    }

    // Enums
    val classSymbol = (type as? IrSimpleType)?.classOrNull
    if (classSymbol != null) {
      val owner = classSymbol.owner
      if (owner.isEnumClass) return true
    }

    // Known stable types
    if (fqn in KNOWN_STABLE_FQNS) return true

    // SDK types (java.*, kotlin.* excluding collections interfaces)
    // Kotlin collections interfaces (List, Set, Map) are NOT stable — they're interfaces
    if (
        fqn == "kotlin.collections.List" ||
            fqn == "kotlin.collections.Set" ||
            fqn == "kotlin.collections.Map" ||
            fqn == "kotlin.collections.MutableList" ||
            fqn == "kotlin.collections.MutableSet" ||
            fqn == "kotlin.collections.MutableMap" ||
            fqn == "kotlin.collections.Collection" ||
            fqn == "kotlin.collections.MutableCollection"
    ) {
      return false
    }

    return false
  }

  private fun IrClass.classFqName(): String? {
    return this.classFqName?.asString()
  }

  private val IrClass.classFqName: FqName?
    get() {
      val segments = mutableListOf<String>()
      var current: IrClass? = this
      while (current != null) {
        segments.add(0, current.name.asString())
        current = current.parent as? IrClass
      }
      val packageFqName =
          generateSequence(this.parent) { (it as? IrDeclaration)?.parent }
              .filterIsInstance<org.jetbrains.kotlin.ir.declarations.IrPackageFragment>()
              .firstOrNull()
              ?.packageFqName ?: return null
      return FqName(packageFqName.asString() + "." + segments.joinToString("."))
    }
}
