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
import org.jetbrains.kotlin.cli.common.messages.CompilerMessageSeverity
import org.jetbrains.kotlin.cli.common.messages.MessageCollector
import org.jetbrains.kotlin.descriptors.ClassKind
import org.jetbrains.kotlin.descriptors.Visibilities
import org.jetbrains.kotlin.fir.FirElement
import org.jetbrains.kotlin.fir.FirSession
import org.jetbrains.kotlin.fir.backend.FirMetadataSource
import org.jetbrains.kotlin.fir.declarations.FirCallableDeclaration
import org.jetbrains.kotlin.fir.declarations.FirDeclaration
import org.jetbrains.kotlin.fir.declarations.FirDeclarationOrigin
import org.jetbrains.kotlin.fir.declarations.FirFile
import org.jetbrains.kotlin.fir.declarations.FirFunction
import org.jetbrains.kotlin.fir.declarations.FirProperty
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
import org.jetbrains.kotlin.fir.symbols.impl.FirCallableSymbol
import org.jetbrains.kotlin.fir.symbols.impl.FirClassSymbol
import org.jetbrains.kotlin.fir.symbols.impl.FirNamedFunctionSymbol
import org.jetbrains.kotlin.fir.symbols.impl.FirPropertySymbol
import org.jetbrains.kotlin.fir.types.ConeErrorType
import org.jetbrains.kotlin.fir.types.ConeKotlinType
import org.jetbrains.kotlin.fir.types.ConeTypeProjection
import org.jetbrains.kotlin.fir.types.coneType
import org.jetbrains.kotlin.fir.types.constructType
import org.jetbrains.kotlin.fir.types.resolvedType
import org.jetbrains.kotlin.fir.types.type
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
import org.jetbrains.kotlin.ir.util.kotlinFqName
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
internal class FirMetadataSanitizerStage(private val repairLog: AbiGenRepairLog) : AbiGenStage {
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
  fun cleanupFirMetadataSources(moduleFragment: IrModuleFragment, session: FirSession) {
    val THROWS_FQ_NAME = FqName("kotlin.jvm.Throws")
    val THROWS_KOTLIN_FQ_NAME = FqName("kotlin.Throws")

    moduleFragment.accept(
        object : IrElementVisitorVoidCompat() {
          override fun visitElement(element: IrElement) {
            element.acceptChildren(this, null)
          }

          override fun visitFile(declaration: IrFile) {
            degradeErrorTypedFileMemberTypes(declaration)
            super.visitFile(declaration)
          }

          override fun visitClass(declaration: IrClass) {
            stripThrowsAndErrorAnnotationsFromDeclaration(declaration)
            stripPrivateSupertypesFromDeclaration(declaration)
            stripErrorTypedPrivateMembersFromDeclaration(declaration)
            super.visitClass(declaration)
          }

          override fun visitSimpleFunction(declaration: IrSimpleFunction) {
            stripThrowsAndErrorAnnotationsFromDeclaration(declaration)
            replaceErrorReturnTypeWithAny(declaration, session)
            replaceErrorValueParameterTypesWithAny(declaration, session)
            super.visitSimpleFunction(declaration)
          }

          override fun visitProperty(declaration: IrProperty) {
            stripThrowsAndErrorAnnotationsFromDeclaration(declaration)
            replaceErrorReturnTypeWithAny(declaration, session)
            super.visitProperty(declaration)
          }

          override fun visitConstructor(declaration: IrConstructor) {
            stripThrowsAndErrorAnnotationsFromDeclaration(declaration)
            replaceErrorValueParameterTypesWithAny(declaration, session)
            super.visitConstructor(declaration)
          }

          private fun stripThrowsAndErrorAnnotationsFromDeclaration(
              declaration: IrDeclarationBase,
          ) {
            val metadataSourceOwner = declaration as? IrMetadataSourceOwner ?: return
            val metadataSource = metadataSourceOwner.metadata ?: return
            val firMetadataSource = metadataSource as? FirMetadataSource ?: return

            stripThrowsFromFirDeclaration(firMetadataSource.fir)
            stripAnnotationsWithErrorsFromFirDeclaration(firMetadataSource.fir)
          }

          // Degrade error-typed NON-API (private/local) members of a class to `Any?` on the FIR
          // metadata source. In source-only ABI, a private member with an inferred type that
          // depends on an unresolvable dependency symbol (e.g.
          // `private val x = Dep.getInstance()` or `private val y = setOf(Dep.CONST)`) gets an
          // error return type, which FirElementSerializer.propertyProto cannot serialize into
          // @Metadata ("Cannot serialize error type").
          //
          // The metadata serializer iterates the class member SYMBOLS
          // (firClass.symbol.declarationSymbols), which can still include members that earlier IR
          // stripping removed from firClass.declarations, so the return type has to be replaced on
          // the symbols' FIR — not just on the declarations list.
          //
          // API members are deliberately left alone: they are part of the ABI, so degrading their
          // type would produce an ABI that lies to consumers (metadata says `Any?` while the
          // bytecode descriptor still says `error/NonExistentClass`), turning a loud build failure
          // into a silent one. Those targets must fix the missing dependency / add an explicit
          // type instead.
          @OptIn(SymbolInternals::class)
          private fun stripErrorTypedPrivateMembersFromDeclaration(declaration: IrClass) {
            val metadataSourceOwner = declaration as? IrMetadataSourceOwner ?: return
            val firMetadataSource = metadataSourceOwner.metadata as? FirMetadataSource ?: return
            val firClass = firMetadataSource.fir as? FirRegularClass ?: return

            // A member of a private/local class is not ABI surface even when it is declared
            // public: no consumer can reference it, so degrading its unserializable type cannot
            // make the ABI lie to anyone. This covers the common
            // `private object Utils { fun get() = Dep.somethingUnresolvable() }` shape, where the
            // member's own visibility is public but the enclosing object is private.
            val classIsNonApi = isClassPrivate(firClass.symbol)

            firClass.symbol.declarationSymbols.forEach { symbol ->
              val decl = (symbol as? FirCallableSymbol<*>)?.fir ?: return@forEach
              if (!classIsNonApi && !isNonApiVisibility(decl)) return@forEach
              // The metadata serializer collects functions/constructors from the class member
              // SCOPE, which references these same symbols' FIR. An error type in any position the
              // serializer reads — the return type, a value parameter (e.g. a parameter typed by a
              // nested enum of a stubbed dependency such as `CdsNavigationBar.Action`), or a
              // property's accessors/backing field — crashes FirElementSerializer the same way, so
              // degrade all of them to `Any?` here. The IR-visitor pass only reaches these via
              // firClass.declarations, which can be missing members served only from the scope.
              degradeErrorTypedPositions(decl, session)
            }

            // Belt-and-suspenders: also cover anything present only in firClass.declarations.
            firClass.declarations.forEach { decl ->
              if (decl is FirCallableDeclaration && (classIsNonApi || isNonApiVisibility(decl))) {
                degradeErrorTypedPositions(decl, session)
              }
            }
          }

          // Same degradation as stripErrorTypedPrivateMembersFromDeclaration, for non-API members
          // the class-scoped pass does not reach: top-level (file-facade) and companion members.
          private fun replaceErrorReturnTypeWithAny(
              declaration: IrDeclarationBase,
              session: FirSession,
          ) {
            val metadataSourceOwner = declaration as? IrMetadataSourceOwner ?: return
            val firMetadataSource = metadataSourceOwner.metadata as? FirMetadataSource ?: return
            val fir = firMetadataSource.fir as? FirCallableDeclaration ?: return
            if (!isNonApiVisibility(fir)) return
            if (!hasErrorReturnType(fir)) return
            runCatching { fir.replaceReturnTypeRef(session.builtinTypes.nullableAnyType) }
          }

          // For a non-API function/constructor with a value parameter whose type resolved to an
          // error type in source-only ABI, replace that parameter type with `Any?` so
          // FirElementSerializer can serialize it instead of crashing in valueParameterProto with
          // "Cannot serialize error type". In source-only ABI a parameter typed by a symbol absent
          // from a stubbed dependency (e.g. a nested enum like `CdsNavigationBar.Action`) resolves
          // to an error type. Mirrors replaceErrorReturnTypeWithAny for parameters.
          private fun replaceErrorValueParameterTypesWithAny(
              declaration: IrDeclarationBase,
              session: FirSession,
          ) {
            val metadataSourceOwner = declaration as? IrMetadataSourceOwner ?: return
            val firMetadataSource = metadataSourceOwner.metadata as? FirMetadataSource ?: return
            val fir = firMetadataSource.fir as? FirFunction ?: return
            if (!isNonApiVisibility(fir)) return
            fir.valueParameters.forEach { param ->
              if (hasErrorReturnType(param)) {
                runCatching { param.replaceReturnTypeRef(session.builtinTypes.nullableAnyType) }
              }
            }
          }

          // The file facade's @Metadata is serialized by FirElementSerializer.packagePartProto,
          // which iterates firFile.declarations. A private top-level member is deleted from the IR
          // by the IR sanitizer before this pass runs, so visitSimpleFunction/visitProperty never
          // reach it, yet the serializer still reads it from the FIR file and crashes on its error
          // type (e.g. `private fun f() = Dep.unresolved()`).
          private fun degradeErrorTypedFileMemberTypes(declaration: IrFile) {
            val firFile = (declaration.metadata as? FirMetadataSource)?.fir as? FirFile ?: return
            firFile.declarations.forEach { decl ->
              if (decl is FirCallableDeclaration && isNonApiVisibility(decl)) {
                degradeErrorTypedPositions(decl, session)
              }
            }
          }

          // Degrade every error-typed position the serializer reads for a callable, not just its
          // own return type. propertyProto serializes the setter's value parameter whenever the
          // accessors are non-default, which is the case for a delegated property
          // (`private var x by AtomicReference(...)` with an unresolved `getValue`), so degrading
          // only the property return type leaves the crash in place.
          private fun degradeErrorTypedPositions(
              decl: FirCallableDeclaration,
              session: FirSession,
          ) {
            if (decl is FirProperty) {
              // The property type, its getter return type, its setter's value parameter and its
              // backing field all describe the SAME type, so they are degraded together: degrading
              // them independently could leave metadata claiming `returnType = Foo` alongside
              // `setterValueParameter = Any?`, a signature the bytecode does not have. The setter's
              // own return type is Unit and is deliberately left alone. Of these, only the property
              // type and the setter value parameter are known to be serialized today; the getter
              // and backing field are included defensively.
              val sameTypePositions =
                  buildList<FirCallableDeclaration> {
                    add(decl)
                    decl.getter?.let { add(it) }
                    decl.setter?.let { addAll(it.valueParameters) }
                    decl.backingField?.let { add(it) }
                  }
              if (sameTypePositions.any { hasErrorReturnType(it) }) {
                sameTypePositions.forEach {
                  runCatching { it.replaceReturnTypeRef(session.builtinTypes.nullableAnyType) }
                }
              }
              return
            }
            // A function's return type and each of its value parameters are independent, so they
            // are degraded one by one to keep the degradation as narrow as possible.
            degradeReturnTypeIfError(decl, session)
            if (decl is FirFunction) {
              decl.valueParameters.forEach { degradeReturnTypeIfError(it, session) }
            }
          }

          private fun degradeReturnTypeIfError(
              decl: FirCallableDeclaration,
              session: FirSession,
          ) {
            if (!hasErrorReturnType(decl)) return
            runCatching { decl.replaceReturnTypeRef(session.builtinTypes.nullableAnyType) }
          }

          // --- @Throws stripping helpers ---

          private fun stripThrowsFromFirDeclaration(
              declaration: org.jetbrains.kotlin.fir.declarations.FirDeclaration?,
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
              declaration: org.jetbrains.kotlin.fir.declarations.FirDeclaration?,
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

                for (classId in strippedSupertypeClassIds) {
                  repairLog.recordStrippedSupertype(
                      firClass.symbol.classId.asString(),
                      "private supertype ${classId.asString()} removed from ABI",
                  )
                }

                convertFirFakeOverridesFromStrippedPrivateSupertypes(
                    firClass,
                    strippedSupertypeClassIds,
                )
              }
            } catch (e: Exception) {
              repairLog.recordFailedRepair(
                  firClass.symbol.classId.asString(),
                  "could not strip private supertypes: ${e.javaClass.simpleName}: ${e.message}",
              )
            }
          }

          private fun getPrivateClassIdFromTypeRef(
              typeRef: org.jetbrains.kotlin.fir.types.FirTypeRef,
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

            val interfaceMethods = collectMethodsFromPrivateInterfaces(
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
                  repairLog.recordFailedRepair(
                      "${firClass.symbol.classId.asString()}.$methodName",
                      "could not register materialized interface method: " +
                          "${e.javaClass.simpleName}: ${e.message}",
                  )
                }
              } else {
                repairLog.recordFailedRepair(
                    "${firClass.symbol.classId.asString()}.$methodName",
                    "could not copy method from stripped private interface",
                )
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
              val newCallableId = CallableId(
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

              if (classSymbol.resolvedStatus.visibility == Visibilities.Local) {
                return true
              }

              // Only a top-level private class is dropped from the ABI; nested private classes
              // are kept so their InnerClasses references resolve. So what matters is whether the
              // outermost enclosing class is private, not whether any enclosing class is. Walk the
              // classId chain by name -- which needs no symbol resolution -- and resolve only the
              // outermost class. Resolving each intermediate enclosing symbol would let one that
              // fails to resolve mask a top-level-private outermost class and leave the supertype
              // dangling.
              var outermostClassId = classId
              while (outermostClassId.outerClassId != null) {
                outermostClassId = outermostClassId.outerClassId!!
              }

              val outermost =
                  if (outermostClassId == classId) {
                    classSymbol
                  } else {
                    session.symbolProvider.getClassLikeSymbolByClassId(outermostClassId)
                        as? FirClassSymbol<*> ?: return false
                  }

              return isClassPrivate(outermost)
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
            },
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

  // A type ref left in an unresolved/inconsistent state after failed inference can throw on
  // coneType access; treat that as an error too.
  private fun hasErrorReturnType(decl: FirCallableDeclaration): Boolean = runCatching {
    decl.returnTypeRef.coneType.containsErrorType()
  }
      .getOrDefault(true)

  // Detect error types anywhere in a type, including nested type arguments. Inference failures
  // often leave the outer type resolved but a type argument as an error type
  // (e.g. `private val x = AtomicReference(Dep.UNRESOLVED)` -> `AtomicReference<ERROR>`), which
  // FirElementSerializer still cannot serialize.
  private fun ConeKotlinType.containsErrorType(): Boolean {
    if (this is ConeErrorType) return true
    return typeArguments.any { projection -> projection.type?.containsErrorType() == true }
  }

  // Only non-API members may have an unserializable error type degraded to `Any?`: they are
  // stripped from the ABI bytecode anyway, so metadata and bytecode stay consistent. Degrading an
  // API member would make the ABI lie to consumers.
  private fun isNonApiVisibility(decl: FirCallableDeclaration): Boolean {
    val visibility = decl.status.visibility
    return visibility == Visibilities.Private ||
        visibility == Visibilities.PrivateToThis ||
        visibility == Visibilities.Local
  }

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
        val owner = property.symbol.callableId.toString()
        try {
          val initializerField = property.javaClass.getDeclaredField("initializer")
          initializerField.isAccessible = true
          initializerField.set(property, null)
          // A const val that loses its initializer is emitted without a ConstantValue attribute,
          // so consumers that constant-fold it fail during their own compile rather than here.
          // That reasoning only applies to a constant a consumer can actually see, hence the
          // visibility, which [ValidationStage] filters on.
          repairLog.recordClearedPropertyInitializer(
              owner,
              if (property.isConst) "const val, no ConstantValue will be emitted"
              else "val initializer discarded",
              isConst = property.isConst,
              consumerVisible =
                  property.status.visibility != Visibilities.Private &&
                      property.status.visibility != Visibilities.PrivateToThis &&
                      property.status.visibility != Visibilities.Local,
          )
        } catch (e: Exception) {
          repairLog.recordFailedRepair(
              owner,
              "could not clear unresolvable initializer: ${e.javaClass.simpleName}: ${e.message}",
          )
        }
      }
      super.visitProperty(property)
    }

    override fun visitRegularClass(regularClass: FirRegularClass) {
      degradeErrorTypedNonApiMemberReturnTypes(regularClass)
      super.visitRegularClass(regularClass)
    }

    // Degrade non-API (private/local) members whose (inferred) type resolved to an error type to
    // `Any?`. Under source-only ABI, an inferred private member whose type flows through a stubbed
    // dependency member can get a ConeErrorType (e.g. `private val x = Dep.factory(...)`,
    // `private val y = AtomicReference(Dep.COMPANION_FIELD)`, or a SAM like
    // `private val z = Dep.Listener { ... }` where the nested type is absent from the stub). Such a
    // member is still serialized into JVM @Metadata (private members are kept when
    // produceHeaderKlib=false), and FirElementSerializer crashes on the error type ("Cannot
    // serialize error type ...").
    //
    // The metadata serializer collects non-static members from the class's UNSUBSTITUTED MEMBER
    // SCOPE (FirElementSerializer.memberDeclarations -> processAllProperties/processAllFunctions),
    // NOT from firClass.declarations. That scope is built during the frontend and references the
    // same FirProperty/FirFunction instances that back firClass.declarations. Merely removing a
    // member from firClass.declarations here does NOT remove it from the already-built scope, so
    // the serializer still sees it (with its error type) and crashes. Instead we DEGRADE the return
    // type on the shared FIR instance to `Any?`; because the scope references the same instance,
    // the
    // serializer then serializes `Any?` and succeeds. This is non-lossy: the member is
    // private/local
    // (not part of the ABI) and is stripped from IR/bytecode by the IR sanitizer and from @Metadata
    // by PrivateMetadataStripper.
    private fun degradeErrorTypedNonApiMemberReturnTypes(firClass: FirRegularClass) {
      val session = firClass.moduleData.session
      firClass.declarations.forEach { decl ->
        if (
            decl is FirCallableDeclaration && isNonApiVisibility(decl) && hasErrorReturnType(decl)
        ) {
          runCatching { decl.replaceReturnTypeRef(session.builtinTypes.nullableAnyType) }
        }
      }
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
        declaration: org.jetbrains.kotlin.fir.declarations.FirDeclaration,
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
internal class IrSanitizerStage(private val repairLog: AbiGenRepairLog) : AbiGenStage {
  override val name = "IrSanitizer"

  /** Create the IR generation extension to be registered during FIR-to-IR conversion. */
  fun createExtension(sourceFiles: List<KtFile>): IrGenerationExtension {
    return NonAbiDeclarationsStrippingIrExtension(sourceFiles, repairLog)
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
 * Answers a single question: does the ABI about to be published differ from what a class-ABI build
 * would have produced, in a way the jar itself cannot reveal?
 *
 * The failure mode this guards against is a *green* build that emits a subtly wrong ABI - a
 * constant holding a placeholder instead of its real value, or a `const val` emitted with no
 * `ConstantValue` attribute at all. Both produce well-formed bytecode, so nothing downstream of
 * here can detect them; the only evidence is what the earlier stages recorded in [AbiGenRepairLog].
 *
 * Rollout: [AbiRepairPolicy.OFF] is the default, so this stage emits nothing unless a target asks
 * for it via `abiValidationMode`. Warnings are not a softer setting here - fbsource builds Kotlin
 * with `-Werror`, so `warn` fails the compile just as `error` does. Enabling it repo-wide therefore
 * has to wait until the repairs it names have been driven out, not the other way round.
 *
 * The cleared-initializer check carries that visibility filter (Assertion 2 below): a `private
 * const val` is not part of any consumer's constant folding, so repairing one silently is not the
 * defect this stage is looking for. [unsoundConstants] needs no equivalent filter - a fabricated
 * constant only ever reaches that path as an annotation argument, which is by construction
 * consumer-visible.
 */
internal class ValidationStage(private val repairLog: AbiGenRepairLog) : AbiGenStage {
  override val name = "Validation"

  fun validate(
      moduleFragment: IrModuleFragment,
      messageCollector: MessageCollector,
      policy: AbiRepairPolicy,
  ) {
    if (policy == AbiRepairPolicy.OFF) return

    // Always emitted, including at zero, so that "no repairs happened" is distinguishable from
    // "validation did not run" when aggregating across a build.
    messageCollector.report(CompilerMessageSeverity.INFO, repairLog.counterLine())

    val severity =
        if (policy == AbiRepairPolicy.ERROR) CompilerMessageSeverity.ERROR
        else CompilerMessageSeverity.WARNING

    // Assertion 1: every synthesised constant has a type consistent with a real declaration.
    // Constants that reached ASSUMED_STRING have a fabricated type, not merely a fabricated value.
    for (constant in repairLog.unsoundConstants()) {
      messageCollector.report(
          severity,
          "Kosabi ABI validation: constant `${constant.classId}.${constant.name}` was emitted " +
              "with an assumed type of `String`. Neither its type nor its value could be " +
              "established, so if the real constant is not a String this ABI is wrong in a way " +
              "that will not surface until a consumer compiles against it.",
      )
    }

    // Assertion 2: no *consumer-visible* const val silently lost its ConstantValue attribute.
    //
    // The visibility filter is not a convenience. Every one of the 12 constants this assertion
    // named on its first repo-wide run was a `private const val` initialised from a constant
    // declared in another buck target. A private companion constant takes part in no consumer's
    // constant folding, so the message's own justification - "consumers that constant-fold it fail
    // during their own compile" - cannot apply to it. Reporting those is a false positive, and
    // since `warn` is fatal under `-Werror` a false positive here is a broken build. What they do
    // expose is real but narrower, and belongs to a different check: Kosabi's const resolver is
    // source-local and cannot read a constant's value off a dependency's ABI.
    for (cleared in repairLog.clearedPropertyInitializers) {
      if (!cleared.isConst) continue
      if (!cleared.consumerVisible) continue
      messageCollector.report(
          severity,
          "Kosabi ABI validation: `${cleared.owner}` is a const val whose initializer could not " +
              "be resolved and was discarded. It will be emitted without a ConstantValue " +
              "attribute, so consumers that constant-fold it fail during their own compile.",
      )
    }

    // Assertion 3: a repair that threw leaves the tree in an unknown state. Unlike a placeholder
    // value there is no claim that can be made about the result at all, which makes this the first
    // check that should be promoted to a hard error once the rate is known to be zero.
    for (failure in repairLog.failedRepairs) {
      messageCollector.report(
          severity,
          "Kosabi ABI validation: repair of `${failure.owner}` failed and was previously " +
              "swallowed: ${failure.detail}. The emitted ABI cannot be trusted.",
      )
    }

    // Assertion 4: no supertype survived that the ABI jar will not contain a class file for.
    // Nested private classes are kept in the ABI, so only a top-level private supertype dangles.
    // Unlike the checks above this one is verifiable from the module itself, so it is checked
    // directly rather than trusted.
    val leakedSupertypes = mutableListOf<String>()
    moduleFragment.accept(
        object : IrElementVisitorVoidCompat() {
          override fun visitElement(element: IrElement) {
            element.acceptChildren(this, null)
          }

          override fun visitClass(declaration: IrClass) {
            for (superType in declaration.superTypes) {
              val superClass =
                  (superType as? org.jetbrains.kotlin.ir.types.IrSimpleType)?.classifier?.owner
                      as? IrClass ?: continue
              if (
                  superClass.parent !is IrClass &&
                      superClass.visibility ==
                          org.jetbrains.kotlin.descriptors.DescriptorVisibilities.PRIVATE
              ) {
                leakedSupertypes.add(
                    "${declaration.kotlinFqName.asString()} -> ${superClass.kotlinFqName.asString()}",
                )
              }
            }
            super.visitClass(declaration)
          }
        },
        null,
    )
    for (leaked in leakedSupertypes) {
      messageCollector.report(
          severity,
          "Kosabi ABI validation: dangling private supertype survived stripping: $leaked",
      )
    }
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
    /** Shared by every stage; the only record of repairs the emitted jar cannot reveal. */
    val repairLog: AbiGenRepairLog = AbiGenRepairLog(),
    val composeAbi: ComposeAbiEmulationStage = ComposeAbiEmulationStage(),
    val bytecodeSanitizer: BytecodeSanitizerStage = BytecodeSanitizerStage(),
) {
  val firMetadataSanitizer: FirMetadataSanitizerStage = FirMetadataSanitizerStage(repairLog)
  val irSanitizer: IrSanitizerStage = IrSanitizerStage(repairLog)
  val validator: ValidationStage = ValidationStage(repairLog)

  val stages: List<AbiGenStage>
    get() = listOf(firMetadataSanitizer, composeAbi, irSanitizer, bytecodeSanitizer, validator)
}
