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
@file:OptIn(
    com.facebook.DirectDeclarationsAccessCompat::class,
)

package com.facebook

import org.jetbrains.kotlin.fir.FirElement
import org.jetbrains.kotlin.fir.backend.FirMetadataSource
import org.jetbrains.kotlin.fir.declarations.FirDeclaration
import org.jetbrains.kotlin.fir.declarations.FirMemberDeclaration
import org.jetbrains.kotlin.fir.declarations.FirRegularClass
import org.jetbrains.kotlin.fir.declarations.utils.isConst
import org.jetbrains.kotlin.fir.expressions.FirAnnotation
import org.jetbrains.kotlin.fir.expressions.FirAnnotationCall
import org.jetbrains.kotlin.fir.expressions.FirErrorExpression
import org.jetbrains.kotlin.fir.expressions.FirFunctionCall
import org.jetbrains.kotlin.fir.expressions.FirGetClassCall
import org.jetbrains.kotlin.fir.expressions.FirNamedArgumentExpression
import org.jetbrains.kotlin.fir.expressions.FirQualifiedAccessExpression
import org.jetbrains.kotlin.fir.expressions.FirVarargArgumentsExpression
import org.jetbrains.kotlin.fir.expressions.FirWrappedArgumentExpression
import org.jetbrains.kotlin.fir.expressions.impl.FirResolvedArgumentList
import org.jetbrains.kotlin.fir.references.FirErrorNamedReference
import org.jetbrains.kotlin.fir.references.FirResolvedNamedReference
import org.jetbrains.kotlin.fir.symbols.SymbolInternals
import org.jetbrains.kotlin.fir.symbols.impl.FirPropertySymbol
import org.jetbrains.kotlin.fir.types.ConeErrorType
import org.jetbrains.kotlin.fir.types.coneType
import org.jetbrains.kotlin.fir.types.resolvedType
import org.jetbrains.kotlin.fir.visitors.FirDefaultVisitorVoid
import org.jetbrains.kotlin.ir.IrElement
import org.jetbrains.kotlin.ir.declarations.IrClass
import org.jetbrains.kotlin.ir.declarations.IrConstructor
import org.jetbrains.kotlin.ir.declarations.IrDeclarationBase
import org.jetbrains.kotlin.ir.declarations.IrFile
import org.jetbrains.kotlin.ir.declarations.IrMetadataSourceOwner
import org.jetbrains.kotlin.ir.declarations.IrModuleFragment
import org.jetbrains.kotlin.ir.declarations.IrProperty
import org.jetbrains.kotlin.ir.declarations.IrSimpleFunction
import org.jetbrains.kotlin.name.FqName

/**
 * Handles all FIR-level metadata sanitization for K2 ABI generation.
 *
 * This includes pre-IR FIR cleanup (stripping annotations with errors, fixing property
 * initializers) and post-IR FIR metadata source cleanup (stripping @Throws with error types,
 * stripping annotations with errors from metadata sources, and stripping private declarations).
 */
@SuppressWarnings("PackageLocationMismatch")
internal class FirMetadataSanitizer {

  // ========== Pre-IR FIR cleanup ==========

  fun stripAnnotationsWithErrors(analysisResults: FirResultCompat) {
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
  fun fixFirErrorExpressionsInPropertyInitializers(analysisResults: FirResultCompat) {
    for (output in analysisResults.outputs) {
      for (firFile in output.fir) {
        firFile.accept(FirPropertyInitializerFixerVisitor())
      }
    }
  }

  // ========== Post-IR FIR metadata source cleanup ==========

  // Strip @Throws annotations with error types from FIR metadata sources attached to IR
  // declarations.
  // We only remove @Throws with error types to avoid breaking Java interop, while keeping valid
  // @Throws annotations so Safe Kotlin plugin can enforce exception handling correctly.
  fun stripThrowsFromFirMetadataSources(moduleFragment: IrModuleFragment) {
    val THROWS_FQ_NAME = FqName("kotlin.jvm.Throws")
    val THROWS_KOTLIN_FQ_NAME = FqName("kotlin.Throws")

    moduleFragment.accept(
        object : IrElementVisitorVoidCompat() {
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

            val annotations = FirReflectionUtils.getMutableAnnotations(declaration) ?: return

            // Only remove @Throws annotations that have error types in their exception class
            // arguments
            val toRemove = annotations.filter { annotation ->
              hasErrorTypeInThrowsAnnotation(
                  annotation,
                  THROWS_FQ_NAME,
                  THROWS_KOTLIN_FQ_NAME,
              )
            }

            if (toRemove.isNotEmpty()) {
              annotations.removeAll(toRemove)
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
  fun stripAnnotationsWithErrorsFromFirMetadataSources(moduleFragment: IrModuleFragment) {
    moduleFragment.accept(
        object : IrElementVisitorVoidCompat() {
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

            val annotations = FirReflectionUtils.getMutableAnnotations(declaration) ?: return

            // Remove any annotation that has error expressions in its arguments
            val toRemove = annotations.filter { annotation ->
              hasErrorExpressionInFirAnnotation(annotation)
            }

            if (toRemove.isNotEmpty()) {
              annotations.removeAll(toRemove)
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

  // Strip private declarations (methods, properties, constructors) from FIR metadata sources.
  // Private members are stripped from IR but can persist in FIR metadata sources (used for
  // Kotlin metadata serialization into @Metadata annotation's d1/d2 arrays). This causes
  // KSP2/DI processors to fail when they see private interface methods.
  fun stripPrivateDeclarationsFromFirMetadataSources(moduleFragment: IrModuleFragment) {
    moduleFragment.accept(
        object : IrElementVisitorVoidCompat() {
          override fun visitElement(element: IrElement) {
            element.acceptChildren(this, null)
          }

          override fun visitClass(declaration: IrClass) {
            stripPrivateDeclarationsFromClass(declaration)
            super.visitClass(declaration)
          }

          private fun stripPrivateDeclarationsFromClass(declaration: IrClass) {
            // Access metadata via IrMetadataSourceOwner interface
            val metadataSourceOwner = declaration as? IrMetadataSourceOwner ?: return
            val metadataSource = metadataSourceOwner.metadata ?: return

            // Check if this is a FirMetadataSource
            val firMetadataSource = metadataSource as? FirMetadataSource ?: return

            // Get the FIR class declaration
            val firClass = firMetadataSource.fir as? FirRegularClass ?: return

            // Get private declarations to remove
            val toRemove = firClass.declarations.filter { decl -> isPrivateFirDeclaration(decl) }

            if (toRemove.isEmpty()) {
              return
            }

            // Use FirReflectionUtils to access the underlying mutable declarations list
            val declarations = FirReflectionUtils.getMutableDeclarations(firClass)
            if (declarations != null) {
              declarations.removeAll(toRemove)
            }
          }

          // Check if a FIR declaration is private (should be stripped from metadata).
          // This is only called for declarations inside a FirRegularClass (class members),
          // never for top-level file declarations — top-level private classes are handled
          // separately by IR stripping in removeNonPublicApi().
          //
          // Private nested classes (FirRegularClass) are kept because the containing class's
          // bytecode and @Metadata reference them. Stripping them from metadata while keeping
          // bytecode references causes inconsistencies.
          private fun isPrivateFirDeclaration(decl: FirDeclaration): Boolean {
            if (decl !is FirMemberDeclaration) return false
            // Keep nested classes — they need to stay in metadata to match bytecode
            if (decl is FirRegularClass) return false

            val visibility = decl.status.visibility
            return visibility == org.jetbrains.kotlin.descriptors.Visibilities.Private ||
                visibility == org.jetbrains.kotlin.descriptors.Visibilities.PrivateToThis ||
                visibility == org.jetbrains.kotlin.descriptors.Visibilities.Local
          }
        },
        null,
    )
  }

  // ========== Error detection helpers ==========

  // Recursively check if a FIR element contains error expressions.
  // This detects both direct FirErrorExpression nodes and qualified accesses with error types.
  // Additionally, for const val references from dependencies, it checks if the initializer
  // is resolvable (not a TODO() or error expression from source-only ABI stubs).
  @OptIn(SymbolInternals::class)
  internal fun hasErrorExpressionRecursive(element: FirElement): Boolean {
    return when (element) {
      is FirErrorExpression -> true
      is FirQualifiedAccessExpression -> {
        try {
          // Check for error types/references
          if (
              element.resolvedType is ConeErrorType ||
                  element.calleeReference is FirErrorNamedReference
          ) {
            return true
          }

          // Check if this is a reference to a const val with an unresolvable initializer.
          // This can happen when the const val is from a source-only ABI stub with TODO().
          val calleeReference = element.calleeReference
          if (calleeReference is FirResolvedNamedReference) {
            val symbol = calleeReference.resolvedSymbol
            if (symbol is FirPropertySymbol && symbol.isConst) {
              // Check if the const val's initializer is resolvable
              val initializer = symbol.fir.initializer
              if (initializer == null) {
                // No initializer means it can't be evaluated
                return true
              }
              // Check if the initializer itself contains errors (like TODO())
              if (isConstValInitializerUnresolvable(initializer)) {
                return true
              }
            }
          }
          false
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
                if (!hasError && hasErrorExpressionRecursive(childElement)) {
                  hasError = true
                }
              }
            }
        )
        hasError
      }
    }
  }

  // Check if a const val initializer is unresolvable (contains TODO(), error expressions, etc.)
  // This is a non-recursive check to avoid infinite loops when dealing with cross-references.
  private fun isConstValInitializerUnresolvable(initializer: FirElement): Boolean {
    return when (initializer) {
      is FirErrorExpression -> true
      is FirFunctionCall -> {
        // Check for TODO() calls which are used in source-only ABI stubs
        try {
          val calleeReference = initializer.calleeReference
          if (calleeReference is FirResolvedNamedReference) {
            val name = calleeReference.name.asString()
            if (name == "TODO") {
              return true
            }
          }
          // Also check if the function call has error type
          initializer.resolvedType is ConeErrorType
        } catch (_: Exception) {
          false
        }
      }
      is FirQualifiedAccessExpression -> {
        try {
          initializer.resolvedType is ConeErrorType ||
              initializer.calleeReference is FirErrorNamedReference
        } catch (_: Exception) {
          false
        }
      }
      else -> false
    }
  }

  // ========== Private visitors ==========

  // Visitor that strips ALL annotations that have error expressions in their arguments.
  // This is more robust than trying to replace error expressions with literals,
  // because FIR mutation is unreliable. If an annotation can't be resolved, we remove it
  // entirely - for ABI generation, it's better to have no annotation than to crash.
  // This matches K1 kosabi behavior where unresolved annotations are naturally omitted.
  private inner class FirAnnotationStrippingVisitor : FirDefaultVisitorVoid() {
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
      val annotations = FirReflectionUtils.getMutableAnnotationCalls(declaration) ?: return

      val toRemove = annotations.filter { annotation -> hasErrorExpressionInAnnotation(annotation) }

      if (toRemove.isNotEmpty()) {
        annotations.removeAll(toRemove)
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
                // 3. Is a TODO() call (stub placeholder that can't be evaluated)
                // Otherwise, the const val has a valid resolvable value.
                if (prop.isConst) {
                  val initializer = prop.initializer
                  if (initializer == null || hasErrorExpression(initializer)) {
                    return true
                  }
                  // Check if the initializer is a TODO() call - stubs replace complex
                  // const val initializers with TODO() which can't be evaluated at compile time
                  if (isTodoCall(initializer)) {
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
        is FirCollectionLiteralCompat -> {
          // Collection/array literal [a, b, c] - check all elements
          try {
            element.argumentList.arguments.any { hasErrorExpression(it) }
          } catch (_: Exception) {
            // If access fails, treat as error to be safe
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

    /**
     * Checks if an expression is a TODO() function call. Stubs replace complex const val
     * initializers with TODO() which cannot be evaluated at compile time during source-only ABI
     * generation.
     */
    private fun isTodoCall(element: FirElement): Boolean {
      if (element !is org.jetbrains.kotlin.fir.expressions.FirFunctionCall) {
        return false
      }
      val calleeRef = element.calleeReference
      if (calleeRef is FirResolvedNamedReference) {
        val name = calleeRef.name.asString()
        return name == "TODO"
      }
      return false
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
        FirReflectionUtils.clearPropertyInitializer(property)
      }
      // Continue visiting nested declarations
      super.visitProperty(property)
    }
  }
}
