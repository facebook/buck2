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
    com.facebook.DeprecatedForRemovalCompilerApiCompat::class,
    com.facebook.DirectDeclarationsAccessCompat::class,
)

package com.facebook

import java.io.File
import org.jetbrains.kotlin.backend.common.extensions.IrGenerationExtension
import org.jetbrains.kotlin.cli.common.messages.MessageCollector
import org.jetbrains.kotlin.descriptors.ClassKind
import org.jetbrains.kotlin.descriptors.Visibilities
import org.jetbrains.kotlin.fir.FirElement
import org.jetbrains.kotlin.fir.FirSession
import org.jetbrains.kotlin.fir.backend.FirMetadataSource
import org.jetbrains.kotlin.fir.declarations.FirDeclaration
import org.jetbrains.kotlin.fir.declarations.FirDeclarationOrigin
import org.jetbrains.kotlin.fir.declarations.FirRegularClass
import org.jetbrains.kotlin.fir.declarations.utils.isConst
import org.jetbrains.kotlin.fir.expressions.FirAnnotation
import org.jetbrains.kotlin.fir.expressions.FirAnnotationCall
import org.jetbrains.kotlin.fir.expressions.FirErrorExpression
import org.jetbrains.kotlin.fir.expressions.FirGetClassCall
import org.jetbrains.kotlin.fir.expressions.FirNamedArgumentExpression
import org.jetbrains.kotlin.fir.expressions.FirQualifiedAccessExpression
import org.jetbrains.kotlin.fir.expressions.FirVarargArgumentsExpression
import org.jetbrains.kotlin.fir.expressions.FirWrappedArgumentExpression
import org.jetbrains.kotlin.fir.expressions.impl.FirResolvedArgumentList
import org.jetbrains.kotlin.fir.moduleData
import org.jetbrains.kotlin.fir.references.FirErrorNamedReference
import org.jetbrains.kotlin.fir.references.FirResolvedNamedReference
import org.jetbrains.kotlin.fir.resolve.providers.symbolProvider
import org.jetbrains.kotlin.fir.serialization.providedDeclarationsForMetadataService
import org.jetbrains.kotlin.fir.symbols.SymbolInternals
import org.jetbrains.kotlin.fir.symbols.impl.FirClassSymbol
import org.jetbrains.kotlin.fir.symbols.impl.FirNamedFunctionSymbol
import org.jetbrains.kotlin.fir.symbols.impl.FirPropertySymbol
import org.jetbrains.kotlin.fir.types.ConeErrorType
import org.jetbrains.kotlin.fir.types.ConeTypeProjection
import org.jetbrains.kotlin.fir.types.coneType
import org.jetbrains.kotlin.fir.types.constructType
import org.jetbrains.kotlin.fir.types.resolvedType
import org.jetbrains.kotlin.fir.visitors.FirDefaultVisitorVoid
import org.jetbrains.kotlin.ir.IrElement
import org.jetbrains.kotlin.ir.declarations.IrClass
import org.jetbrains.kotlin.ir.declarations.IrConstructor
import org.jetbrains.kotlin.ir.declarations.IrDeclarationBase
import org.jetbrains.kotlin.ir.declarations.IrMetadataSourceOwner
import org.jetbrains.kotlin.ir.declarations.IrModuleFragment
import org.jetbrains.kotlin.ir.declarations.IrProperty
import org.jetbrains.kotlin.ir.declarations.IrSimpleFunction
import org.jetbrains.kotlin.name.CallableId
import org.jetbrains.kotlin.name.ClassId
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.psi.KtFile

/**
 * FIR metadata sanitization stage.
 *
 * Handles both pre-IR FIR tree cleanup and post-IR FIR metadata source cleanup:
 * - Strip annotations with errors and fix property initializers (FIR tree, pre-IR, single pass)
 * - Strip @Throws from FIR metadata sources (post-IR)
 * - Strip annotations with errors from FIR metadata sources (post-IR)
 * - Strip private supertypes + fake override conversion (post-IR)
 */
internal class FirMetadataSanitizerStage : AbiGenStage {
  override val name = "FirMetadataSanitizer"

  /**
   * Pre-IR: cleanup the FIR tree before FIR-to-IR conversion in a single pass.
   *
   * Combines two operations that both walk the FIR tree:
   * 1. Strip ALL annotations that have error expressions in their arguments.
   * 2. Fix property initializers containing error expressions (clear them).
   */
  fun cleanupFirTree(firResult: FirResultCompat) {
    for (output in firResult.outputs) {
      for (firFile in output.fir) {
        firFile.accept(FirSanitizingVisitor())
      }
    }
  }

  /**
   * Post-IR: cleanup FIR metadata sources attached to IR declarations in a single IR tree walk.
   *
   * Combines three operations that all walk the IR module visiting classes, functions, properties,
   * and constructors to access FirMetadataSource:
   * 1. Strip @Throws annotations with error types from FIR metadata sources.
   * 2. Strip ALL annotations with error expressions from FIR metadata sources.
   * 3. Strip PRIVATE supertypes from FIR metadata sources.
   */
  fun cleanupFirMetadataSources(moduleFragment: IrModuleFragment) {
    val THROWS_FQ_NAME = FqName("kotlin.jvm.Throws")
    val THROWS_KOTLIN_FQ_NAME = FqName("kotlin.Throws")

    moduleFragment.accept(
        object : IrElementVisitorVoidCompat() {
          override fun visitElement(element: IrElement) {
            element.acceptChildren(this, null)
          }

          override fun visitClass(declaration: IrClass) {
            stripThrowsAndErrorAnnotationsFromDeclaration(declaration)
            stripPrivateSupertypesFromDeclaration(declaration)
            super.visitClass(declaration)
          }

          override fun visitSimpleFunction(declaration: IrSimpleFunction) {
            stripThrowsAndErrorAnnotationsFromDeclaration(declaration)
            super.visitSimpleFunction(declaration)
          }

          override fun visitProperty(declaration: IrProperty) {
            stripThrowsAndErrorAnnotationsFromDeclaration(declaration)
            super.visitProperty(declaration)
          }

          override fun visitConstructor(declaration: IrConstructor) {
            stripThrowsAndErrorAnnotationsFromDeclaration(declaration)
            super.visitConstructor(declaration)
          }

          private fun stripThrowsAndErrorAnnotationsFromDeclaration(
              declaration: IrDeclarationBase
          ) {
            val metadataSourceOwner = declaration as? IrMetadataSourceOwner ?: return
            val metadataSource = metadataSourceOwner.metadata ?: return
            val firMetadataSource = metadataSource as? FirMetadataSource ?: return

            stripThrowsFromFirDeclaration(firMetadataSource.fir)
            stripAnnotationsWithErrorsFromFirDeclaration(firMetadataSource.fir)
          }

          // --- @Throws stripping helpers ---

          private fun stripThrowsFromFirDeclaration(
              declaration: org.jetbrains.kotlin.fir.declarations.FirDeclaration?
          ) {
            if (declaration == null) return

            try {
              val annotationsField = declaration.javaClass.getDeclaredField("annotations")
              annotationsField.isAccessible = true
              val annotationsWrapper = annotationsField.get(declaration) ?: return

              val listField = annotationsWrapper.javaClass.getDeclaredField("list")
              listField.isAccessible = true
              @Suppress("UNCHECKED_CAST")
              val annotations =
                  listField.get(annotationsWrapper) as? MutableList<FirAnnotation> ?: return

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
            } catch (_: Exception) {
              // If reflection fails, skip this declaration
            }
          }

          private fun hasErrorTypeInThrowsAnnotation(
              annotation: FirAnnotation,
              throwsFqName: FqName,
              throwsKotlinFqName: FqName,
          ): Boolean {
            val annotationType = annotation.annotationTypeRef.coneType
            val fqName =
                (annotationType as? org.jetbrains.kotlin.fir.types.ConeClassLikeType)
                    ?.lookupTag
                    ?.classId
                    ?.asSingleFqName()

            if (fqName != throwsFqName && fqName != throwsKotlinFqName) {
              return false
            }

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

          // --- Annotation error stripping helpers ---

          private fun stripAnnotationsWithErrorsFromFirDeclaration(
              declaration: org.jetbrains.kotlin.fir.declarations.FirDeclaration?
          ) {
            if (declaration == null) return

            try {
              val annotationsField = declaration.javaClass.getDeclaredField("annotations")
              annotationsField.isAccessible = true
              val annotationsWrapper = annotationsField.get(declaration) ?: return

              val listField = annotationsWrapper.javaClass.getDeclaredField("list")
              listField.isAccessible = true
              @Suppress("UNCHECKED_CAST")
              val annotations =
                  listField.get(annotationsWrapper) as? MutableList<FirAnnotation> ?: return

              val toRemove = annotations.filter { annotation ->
                hasErrorExpressionInFirAnnotation(annotation)
              }

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

          // --- Private supertype stripping helpers ---

          @OptIn(SymbolInternals::class)
          private fun stripPrivateSupertypesFromDeclaration(declaration: IrClass) {
            val metadataSourceOwner = declaration as? IrMetadataSourceOwner ?: return
            val metadataSource = metadataSourceOwner.metadata ?: return
            val firMetadataSource = metadataSource as? FirMetadataSource ?: return
            val firClass = firMetadataSource.fir as? FirRegularClass ?: return

            val strippedSupertypeClassIds = mutableSetOf<ClassId>()

            try {
              val superTypeRefsField = firClass.javaClass.getDeclaredField("superTypeRefs")
              superTypeRefsField.isAccessible = true
              val superTypeRefsValue = superTypeRefsField.get(firClass) ?: return

              @Suppress("UNCHECKED_CAST")
              val superTypeRefs: MutableList<org.jetbrains.kotlin.fir.types.FirTypeRef> =
                  when (superTypeRefsValue) {
                    is MutableList<*> ->
                        superTypeRefsValue as MutableList<org.jetbrains.kotlin.fir.types.FirTypeRef>
                    else -> {
                      val listField =
                          superTypeRefsValue.javaClass.declaredFields.find { it.name == "list" }
                              ?: return
                      listField.isAccessible = true
                      listField.get(superTypeRefsValue)
                          as? MutableList<org.jetbrains.kotlin.fir.types.FirTypeRef> ?: return
                    }
                  }

              val toRemove = superTypeRefs.filter { typeRef ->
                isPrivateSupertype(typeRef, firClass)
              }

              for (typeRef in toRemove) {
                val classId = getPrivateClassIdFromTypeRef(typeRef)
                if (classId != null) {
                  strippedSupertypeClassIds.add(classId)
                }
              }

              if (toRemove.isNotEmpty()) {
                superTypeRefs.removeAll(toRemove)

                convertFirFakeOverridesFromStrippedPrivateSupertypes(
                    firClass,
                    strippedSupertypeClassIds,
                )
              }
            } catch (e: Exception) {
              // Reflection failures are silently ignored
            }
          }

          private fun getPrivateClassIdFromTypeRef(
              typeRef: org.jetbrains.kotlin.fir.types.FirTypeRef
          ): ClassId? {
            val coneType =
                (typeRef as? org.jetbrains.kotlin.fir.types.FirResolvedTypeRef)?.coneType
                    ?: return null
            return (coneType as? org.jetbrains.kotlin.fir.types.ConeClassLikeType)
                ?.lookupTag
                ?.classId
          }

          @OptIn(SymbolInternals::class)
          private fun convertFirFakeOverridesFromStrippedPrivateSupertypes(
              firClass: FirRegularClass,
              strippedSupertypeClassIds: Set<ClassId>,
          ) {
            if (strippedSupertypeClassIds.isEmpty()) return

            val interfaceMethods =
                collectMethodsFromPrivateInterfaces(
                    firClass.moduleData.session,
                    strippedSupertypeClassIds,
                )

            if (interfaceMethods.isEmpty()) return

            val existingMethodNames =
                firClass.declarations
                    .filterIsInstance<FirNamedFunctionCompat>()
                    .map { it.name.asString() }
                    .toSet()

            for (interfaceMethod in interfaceMethods) {
              val methodName = interfaceMethod.name.asString()
              if (methodName in existingMethodNames) continue

              val copiedMethod = copyPrivateInterfaceMethodToClass(interfaceMethod, firClass)
              if (copiedMethod != null) {
                (firClass.declarations as MutableList<FirDeclaration>).add(copiedMethod)
                try {
                  firClass.moduleData.session.providedDeclarationsForMetadataService
                      .registerDeclaration(copiedMethod)
                } catch (e: Exception) {
                  // Registration failure is silently ignored
                }
              }
            }
          }

          @OptIn(SymbolInternals::class)
          private fun copyPrivateInterfaceMethodToClass(
              interfaceMethod: FirNamedFunctionCompat,
              targetClass: FirRegularClass,
          ): FirNamedFunctionCompat? {
            return try {
              val targetClassId = targetClass.symbol.classId
              val newCallableId =
                  CallableId(
                      targetClassId.packageFqName,
                      targetClassId.relativeClassName,
                      interfaceMethod.name,
                  )
              buildNamedFunctionCopyCompat(interfaceMethod) {
                origin = FirDeclarationOrigin.Source
                symbol = FirNamedFunctionSymbol(newCallableId)
                dispatchReceiverType =
                    targetClass.symbol.constructType(
                        ConeTypeProjection.EMPTY_ARRAY,
                        isMarkedNullable = false,
                    )
              }
            } catch (e: Exception) {
              null
            }
          }

          @OptIn(SymbolInternals::class)
          private fun collectMethodsFromPrivateInterfaces(
              session: FirSession,
              interfaceClassIds: Set<ClassId>,
          ): List<FirNamedFunctionCompat> {
            val methods = mutableListOf<FirNamedFunctionCompat>()
            for (classId in interfaceClassIds) {
              val classSymbol =
                  session.symbolProvider.getClassLikeSymbolByClassId(classId) as? FirClassSymbol<*>
                      ?: continue
              val firClass = classSymbol.fir as? FirRegularClass ?: continue
              if (firClass.classKind != ClassKind.INTERFACE) continue

              for (decl in firClass.declarations) {
                if (decl is FirNamedFunctionCompat) {
                  val visibility = decl.status.visibility
                  if (visibility == Visibilities.Public || visibility == Visibilities.Protected) {
                    methods.add(decl)
                  }
                }
              }
            }
            return methods
          }

          private fun isPrivateSupertype(
              typeRef: org.jetbrains.kotlin.fir.types.FirTypeRef,
              firClass: FirRegularClass,
          ): Boolean {
            try {
              val coneType =
                  (typeRef as? org.jetbrains.kotlin.fir.types.FirResolvedTypeRef)?.coneType
                      ?: return false
              val classId =
                  (coneType as? org.jetbrains.kotlin.fir.types.ConeClassLikeType)
                      ?.lookupTag
                      ?.classId ?: return false

              val session = firClass.moduleData.session
              val classSymbol =
                  session.symbolProvider.getClassLikeSymbolByClassId(classId) as? FirClassSymbol<*>
                      ?: return false

              if (isClassPrivate(classSymbol)) {
                return true
              }

              var outerClassId = classId.outerClassId
              while (outerClassId != null) {
                val outerSymbol =
                    session.symbolProvider.getClassLikeSymbolByClassId(outerClassId)
                        as? FirClassSymbol<*>
                if (outerSymbol != null && isClassPrivate(outerSymbol)) {
                  return true
                }
                outerClassId = outerClassId.outerClassId
              }

              return false
            } catch (e: Exception) {
              return false
            }
          }

          private fun isClassPrivate(classSymbol: FirClassSymbol<*>): Boolean {
            val visibility = classSymbol.resolvedStatus.visibility
            return visibility == Visibilities.Private || visibility == Visibilities.Local
          }
        },
        null,
    )
  }

  // --- Public utility methods ---

  // Recursively check if a FIR element contains error expressions.
  // Used by K2JvmAbiFirAnalysisHandlerExtension to check const val initializer resolvability.
  @OptIn(SymbolInternals::class)
  fun hasErrorExpressionRecursive(element: FirElement): Boolean {
    return when (element) {
      is FirErrorExpression -> true
      is FirQualifiedAccessExpression -> {
        try {
          if (
              element.resolvedType is ConeErrorType ||
                  element.calleeReference is FirErrorNamedReference
          ) {
            return true
          }

          val calleeReference = element.calleeReference
          if (calleeReference is FirResolvedNamedReference) {
            val symbol = calleeReference.resolvedSymbol
            if (symbol is FirPropertySymbol && symbol.isConst) {
              val initializer = symbol.fir.initializer
              if (initializer == null) {
                return true
              }
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

  private fun isConstValInitializerUnresolvable(initializer: FirElement): Boolean {
    return when (initializer) {
      is FirErrorExpression -> true
      is org.jetbrains.kotlin.fir.expressions.FirFunctionCall -> {
        try {
          val calleeReference = initializer.calleeReference
          if (calleeReference is FirResolvedNamedReference) {
            val name = calleeReference.name.asString()
            if (name == "TODO") {
              return true
            }
          }
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

  // --- FIR tree sanitizing visitor for cleanupFirTree ---

  /**
   * Single-pass visitor that sanitizes FIR tree:
   * 1. Strips ALL annotations that have error expressions in their arguments.
   * 2. Clears property initializers containing error expressions.
   */
  private inner class FirSanitizingVisitor : FirDefaultVisitorVoid() {
    override fun visitElement(element: FirElement) {
      if (element is org.jetbrains.kotlin.fir.declarations.FirDeclaration) {
        stripAnnotationsWithErrors(element)
      }
      element.acceptChildren(this)
    }

    override fun visitProperty(property: org.jetbrains.kotlin.fir.declarations.FirProperty) {
      val initializer = property.initializer
      if (initializer != null && hasErrorExpressionRecursive(initializer)) {
        try {
          val initializerField = property.javaClass.getDeclaredField("initializer")
          initializerField.isAccessible = true
          initializerField.set(property, null)
        } catch (_: Exception) {
          // If reflection fails, skip this property
        }
      }
      super.visitProperty(property)
    }

    private fun findFieldInHierarchy(
        clazz: Class<*>,
        fieldName: String,
    ): java.lang.reflect.Field? {
      var current: Class<*>? = clazz
      while (current != null) {
        val field = current.declaredFields.find { it.name == fieldName }
        if (field != null) return field
        current = current.superclass
      }
      return null
    }

    private fun stripAnnotationsWithErrors(
        declaration: org.jetbrains.kotlin.fir.declarations.FirDeclaration
    ) {
      try {
        val annotationsField = findFieldInHierarchy(declaration.javaClass, "annotations") ?: return
        annotationsField.isAccessible = true
        @Suppress("UNCHECKED_CAST")
        val annotations =
            annotationsField.get(declaration) as? MutableList<FirAnnotationCall> ?: return

        val toRemove = annotations.filter { annotation ->
          hasErrorExpressionInAnnotation(annotation)
        }

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
          hasErrorExpression(element.expression)
        }
        is FirVarargArgumentsExpression -> {
          element.arguments.any { hasErrorExpression(it) }
        }
        is FirQualifiedAccessExpression -> {
          try {
            if (
                element.resolvedType is ConeErrorType ||
                    element.calleeReference is FirErrorNamedReference
            ) {
              return true
            }

            val calleeRef = element.calleeReference
            if (calleeRef is FirResolvedNamedReference) {
              val symbol = calleeRef.resolvedSymbol
              if (symbol is FirPropertySymbol) {
                val prop = symbol.fir
                if (prop.isConst) {
                  val initializer = prop.initializer
                  if (initializer == null || hasErrorExpression(initializer)) {
                    return true
                  }
                  if (isTodoCall(initializer)) {
                    return true
                  }
                  return false
                }
              }
            }
            false
          } catch (_: Exception) {
            true
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
            true
          }
        }
        is FirCollectionLiteralCompat -> {
          try {
            element.argumentList.arguments.any { hasErrorExpression(it) }
          } catch (_: Exception) {
            true
          }
        }
        is org.jetbrains.kotlin.fir.expressions.FirFunctionCall -> {
          try {
            element.resolvedType is ConeErrorType ||
                element.calleeReference is FirErrorNamedReference
          } catch (_: Exception) {
            true
          }
        }
        else -> false
      }
    }

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
}

/**
 * IR sanitization stage.
 *
 * Wraps the NonAbiDeclarationsStrippingIrExtension which:
 * - Removes stub source files from IR
 * - Strips SOURCE retention annotations
 * - Strips private declarations (removeNonPublicApi)
 * - Strips private supertypes and converts fake overrides
 * - Stubs method bodies
 */
internal class IrSanitizerStage : AbiGenStage {
  override val name = "IrSanitizer"

  /** Create the IR generation extension to be registered during FIR-to-IR conversion. */
  fun createExtension(sourceFiles: List<KtFile>): IrGenerationExtension {
    return NonAbiDeclarationsStrippingIrExtension(sourceFiles)
  }
}

/**
 * Bytecode sanitization stage.
 *
 * ASM-based post-processing of generated .class files:
 * - Strip @Throws annotations from RuntimeInvisibleAnnotations
 * - Strip private declarations from @Metadata annotation
 */
internal class BytecodeSanitizerStage : AbiGenStage {
  override val name = "BytecodeSanitizer"

  private val transformers = listOf(ThrowsAnnotationStripper(), PrivateMetadataStripper())

  /** Transform class bytecode in-memory, applying all sanitization transformers. */
  fun transform(bytes: ByteArray): ByteArray {
    var result = bytes
    for (transformer in transformers) {
      val transformed = transformer.transform(result)
      if (transformed != null) {
        result = transformed
      }
    }
    return result
  }

  /** Process all .class files in the output directory (disk-based). */
  fun process(outputDir: File) {
    outputDir
        .walkTopDown()
        .filter { it.extension == "class" }
        .forEach { classFile ->
          var bytes = classFile.readBytes()
          var modified = false
          for (transformer in transformers) {
            val result = transformer.transform(bytes)
            if (result != null) {
              bytes = result
              modified = true
            }
          }
          if (modified) {
            classFile.writeBytes(bytes)
          }
        }
  }
}

/**
 * Validation stage.
 *
 * Checks for leftover references to private supertypes or error annotations that might have slipped
 * through the sanitization stages.
 */
internal class ValidationStage : AbiGenStage {
  override val name = "Validation"

  fun validate(moduleFragment: IrModuleFragment, messageCollector: MessageCollector) {
    // Placeholder for future validation checks.
    // Potential checks:
    // - Verify no IrClass has private supertypes remaining
    // - Verify no FirMetadataSource has annotations with error expressions
    // - Verify no bytecode references to stripped classes
  }
}

/**
 * Pipeline runner that orchestrates the ABI generation stages in the correct order.
 *
 * The pipeline stages are:
 * 1. FIR pre-IR cleanup (FirMetadataSanitizerStage.cleanupFirTree)
 * 2. FIR-to-IR conversion with IR sanitizer extension
 * 3. FIR metadata post-IR cleanup (FirMetadataSanitizerStage.cleanupFirMetadataSources)
 * 4. Code generation + in-memory bytecode transforms + write to disk
 * 5. Validation (ValidationStage.validate)
 *
 * Steps 2 and 4 are handled by the caller (K2JvmAbiFirAnalysisHandlerExtension) since they involve
 * compiler infrastructure (FIR-to-IR conversion, code generation).
 */
internal class AbiGenPipeline(
    val firMetadataSanitizer: FirMetadataSanitizerStage = FirMetadataSanitizerStage(),
    val composeAbi: ComposeAbiEmulationStage = ComposeAbiEmulationStage(),
    val irSanitizer: IrSanitizerStage = IrSanitizerStage(),
    val bytecodeSanitizer: BytecodeSanitizerStage = BytecodeSanitizerStage(),
    val validator: ValidationStage = ValidationStage(),
) {
  val stages: List<AbiGenStage>
    get() = listOf(firMetadataSanitizer, composeAbi, irSanitizer, bytecodeSanitizer, validator)
}
