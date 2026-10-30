/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

@file:OptIn(org.jetbrains.kotlin.fir.symbols.SymbolInternals::class)

package com.facebook.buck.jvm.kotlin.compilerplugins.common

import java.util.Collections
import java.util.IdentityHashMap
import org.jetbrains.kotlin.descriptors.ClassKind
import org.jetbrains.kotlin.descriptors.Visibilities
import org.jetbrains.kotlin.fir.FirElement
import org.jetbrains.kotlin.fir.FirSession
import org.jetbrains.kotlin.fir.declarations.FirAnonymousFunction
import org.jetbrains.kotlin.fir.declarations.FirAnonymousObject
import org.jetbrains.kotlin.fir.declarations.FirDeclarationOrigin
import org.jetbrains.kotlin.fir.declarations.FirFile
import org.jetbrains.kotlin.fir.declarations.FirFunction
import org.jetbrains.kotlin.fir.declarations.FirMemberDeclaration
import org.jetbrains.kotlin.fir.declarations.FirRegularClass
import org.jetbrains.kotlin.fir.declarations.FirResolvedDeclarationStatus
import org.jetbrains.kotlin.fir.declarations.FirTypeAlias
import org.jetbrains.kotlin.fir.declarations.utils.effectiveVisibility
import org.jetbrains.kotlin.fir.declarations.utils.isConst
import org.jetbrains.kotlin.fir.declarations.utils.isInline
import org.jetbrains.kotlin.fir.declarations.utils.isInlineOrValue
import org.jetbrains.kotlin.fir.declarations.utils.isSuspend
import org.jetbrains.kotlin.fir.expressions.FirAnnotation
import org.jetbrains.kotlin.fir.expressions.FirAnonymousFunctionExpression
import org.jetbrains.kotlin.fir.expressions.FirCall
import org.jetbrains.kotlin.fir.expressions.FirCallableReferenceAccess
import org.jetbrains.kotlin.fir.expressions.FirErrorExpression
import org.jetbrains.kotlin.fir.expressions.FirExpression
import org.jetbrains.kotlin.fir.expressions.FirQualifiedAccessExpression
import org.jetbrains.kotlin.fir.expressions.FirResolvable
import org.jetbrains.kotlin.fir.expressions.FirResolvedQualifier
import org.jetbrains.kotlin.fir.expressions.FirWhenExpression
import org.jetbrains.kotlin.fir.expressions.resolvedArgumentMapping
import org.jetbrains.kotlin.fir.references.FirErrorNamedReference
import org.jetbrains.kotlin.fir.references.FirResolvedNamedReference
import org.jetbrains.kotlin.fir.resolve.providers.firProvider
import org.jetbrains.kotlin.fir.resolve.providers.symbolProvider
import org.jetbrains.kotlin.fir.symbols.FirBasedSymbol
import org.jetbrains.kotlin.fir.symbols.impl.FirAnonymousFunctionSymbol
import org.jetbrains.kotlin.fir.symbols.impl.FirCallableSymbol
import org.jetbrains.kotlin.fir.symbols.impl.FirClassLikeSymbol
import org.jetbrains.kotlin.fir.symbols.impl.FirNamedFunctionSymbol
import org.jetbrains.kotlin.fir.symbols.impl.FirPropertySymbol
import org.jetbrains.kotlin.fir.symbols.impl.FirTypeParameterSymbol
import org.jetbrains.kotlin.fir.symbols.impl.FirValueParameterSymbol
import org.jetbrains.kotlin.fir.symbols.impl.FirVariableSymbol
import org.jetbrains.kotlin.fir.types.ConeCapturedType
import org.jetbrains.kotlin.fir.types.ConeClassLikeType
import org.jetbrains.kotlin.fir.types.ConeDefinitelyNotNullType
import org.jetbrains.kotlin.fir.types.ConeFlexibleType
import org.jetbrains.kotlin.fir.types.ConeIntegerLiteralType
import org.jetbrains.kotlin.fir.types.ConeIntersectionType
import org.jetbrains.kotlin.fir.types.ConeKotlinType
import org.jetbrains.kotlin.fir.types.ConeKotlinTypeProjection
import org.jetbrains.kotlin.fir.types.ConeStarProjection
import org.jetbrains.kotlin.fir.types.ConeTypeParameterType
import org.jetbrains.kotlin.fir.types.FirErrorTypeRef
import org.jetbrains.kotlin.fir.types.FirResolvedTypeRef
import org.jetbrains.kotlin.fir.types.coneType
import org.jetbrains.kotlin.fir.types.isMarkedNullable
import org.jetbrains.kotlin.fir.types.resolvedType
import org.jetbrains.kotlin.fir.unwrapFakeOverrides
import org.jetbrains.kotlin.fir.visitors.FirVisitorVoid
import org.jetbrains.kotlin.name.ClassId
import org.jetbrains.kotlin.name.FqName

/**
 * Decides whether the body of a non-private inline function can be kept in the source-only ABI.
 *
 * Every declaration the function's signature and body reference (calls, types, implicit receivers
 * and type arguments) must be declared in this module's own Kotlin sources or be a library
 * declaration that [isLibraryDeclarationAllowed] accepts; unresolved references, and declarations
 * of Kosabi stub files, never qualify. With [lowerableOnly], the body must also have a shape that
 * k2-jvm-abi-gen can emit as a plain method body: see [hasLowerableShape].
 *
 * @param isLibraryDeclarationAllowed called with the fully qualified name of a library class, or of
 *   a top-level library function or property (then `isTopLevelMember` is true).
 * @param isStubFile true for the Kosabi stub files of the ABI compilation.
 */
class InlineBodyKeepability(
    private val session: FirSession,
    private val lowerableOnly: Boolean,
    private val isStubFile: (FirFile) -> Boolean = { false },
    private val isLibraryDeclarationAllowed: (fqName: String, isTopLevelMember: Boolean) -> Boolean,
) {
  private val verdicts = HashMap<FirFunction, Boolean>()
  private val inProgress = HashSet<FirFunction>()

  /** The functions the applicability check reports as inline functions. */
  fun isCandidate(function: FirFunction): Boolean =
      function.symbol is FirNamedFunctionSymbol &&
          function.isInline &&
          !function.effectiveVisibility.privateApi

  fun isKeepable(function: FirFunction): Boolean {
    verdicts[function]?.let {
      return it
    }
    // Inline functions cannot call each other in a cycle, so a cycle strips.
    if (!inProgress.add(function)) return false
    val result =
        try {
          compute(function)
        } finally {
          inProgress.remove(function)
        }
    verdicts[function] = result
    return result
  }

  private fun compute(function: FirFunction): Boolean {
    if (!isCandidate(function) || function.body == null) return false
    if (lowerableOnly && !hasLowerableShape(function)) return false
    val visitor = Visitor(function)
    function.accept(visitor)
    return visitor.keepable
  }

  /**
   * Shapes that the ABI cannot carry as one plain method body: `suspend` (a state-machine twin),
   * default values (a `$default` twin), value classes in the signature (mangled names), members of
   * value classes or interfaces, and, in the body (see [Visitor]), anonymous objects and local
   * declarations, lambdas that are not inlined, callable references, `when` over an enum and
   * `entries` of a library enum (classes generated next to the caller), non-Kotlin annotations
   * (compiler plugins rewrite annotated code), reads of own `const val`s and own inline properties,
   * calls to own inline functions that are not kept, and own declarations the ABI does not carry
   * with the same signature (private ones, and protected ones reached through a synthetic
   * accessor).
   */
  private fun hasLowerableShape(function: FirFunction): Boolean {
    if (function.isSuspend || function.valueParameters.any { it.defaultValue != null }) return false
    val owner = function.symbol.callableId?.classId?.let(::regularClass)
    if (
        owner != null &&
            (owner.isInlineOrValue ||
                owner.classKind == ClassKind.INTERFACE ||
                owner.classKind == ClassKind.ANNOTATION_CLASS)
    ) {
      return false
    }
    val signature =
        function.valueParameters.map { it.returnTypeRef } +
            listOfNotNull(function.receiverParameter?.typeRef, function.returnTypeRef)
    return signature.none {
      it.coneTypeOrNull?.classId?.let(::regularClass)?.isInlineOrValue == true
    }
  }

  private val FirElement.coneTypeOrNull: ConeKotlinType?
    get() = (this as? FirResolvedTypeRef)?.coneType

  private val ConeKotlinType.classId: ClassId?
    get() =
        when (this) {
          is ConeClassLikeType -> lookupTag.classId
          is ConeFlexibleType -> lowerBound.classId
          is ConeDefinitelyNotNullType -> original.classId
          else -> null
        }

  private fun regularClass(classId: ClassId): FirRegularClass? =
      session.symbolProvider.getClassLikeSymbolByClassId(classId)?.fir as? FirRegularClass

  private fun isOwn(symbol: FirBasedSymbol<*>, origin: FirDeclarationOrigin): Boolean {
    if (!origin.fromSource || origin is FirDeclarationOrigin.Java) return false
    if (origin is FirDeclarationOrigin.Plugin) return false
    val file =
        when (symbol) {
          is FirClassLikeSymbol<*> -> session.firProvider.getFirClassifierContainerFileIfAny(symbol)
          is FirCallableSymbol<*> -> session.firProvider.getFirCallableContainerFile(symbol)
          else -> null
        }
    return file != null && !isStubFile(file)
  }

  private fun isAllowedClass(classId: ClassId): Boolean {
    // Anonymous objects and local classes are part of the body; with lowerableOnly they are
    // rejected.
    if (classId.isLocal) return true
    val symbol = session.symbolProvider.getClassLikeSymbolByClassId(classId) ?: return false
    if (symbol.origin.fromSource) {
      // With lowerableOnly: the ABI drops private classes.
      val private = (symbol.fir as? FirMemberDeclaration)?.effectiveVisibility?.privateApi != false
      return isOwn(symbol, symbol.origin) && !(lowerableOnly && private)
    }
    return isLibraryDeclarationAllowed(classId.asSingleFqName().asString(), false)
  }

  private fun isAllowedType(
      type: ConeKotlinType,
      visited: MutableSet<ConeCapturedType> = Collections.newSetFromMap(IdentityHashMap()),
  ): Boolean =
      when (type) {
        is ConeClassLikeType ->
            isAllowedClass(type.lookupTag.classId) &&
                type.typeArguments.all {
                  it is ConeStarProjection ||
                      (it is ConeKotlinTypeProjection && isAllowedType(it.type, visited))
                }
        is ConeTypeParameterType,
        is ConeIntegerLiteralType -> true
        is ConeFlexibleType ->
            isAllowedType(type.lowerBound, visited) && isAllowedType(type.upperBound, visited)
        is ConeDefinitelyNotNullType -> isAllowedType(type.original, visited)
        is ConeIntersectionType -> type.intersectedTypes.all { isAllowedType(it, visited) }
        is ConeCapturedType ->
            !visited.add(type) ||
                ((type.constructor.projection as? ConeKotlinTypeProjection)?.let {
                  isAllowedType(it.type, visited)
                } != false &&
                    type.constructor.supertypes.orEmpty().all { isAllowedType(it, visited) })
        // Error types and anything not modelled here.
        else -> false
      }

  private inner class Visitor(private val function: FirFunction) : FirVisitorVoid() {
    var keepable = true
    // Lambdas passed to an inline parameter of an inline function generate no class.
    private val inlinedLambdas = HashSet<FirAnonymousFunction>()

    override fun visitElement(element: FirElement) {
      if (!keepable) return
      keepable = isAllowed(element) && (!lowerableOnly || isLowerable(element))
      element.acceptChildren(this)
    }

    private fun isAllowed(element: FirElement): Boolean =
        element !is FirErrorExpression &&
            element !is FirErrorTypeRef &&
            (element !is FirResolvedTypeRef || isAllowedType(element.coneType)) &&
            (element !is FirExpression ||
                runCatching { element.resolvedType }.getOrNull()?.let(::isAllowedType) == true) &&
            (element !is FirQualifiedAccessExpression ||
                listOfNotNull(element.dispatchReceiver, element.extensionReceiver).all {
                  isAllowedType(it.resolvedType)
                }) &&
            (element !is FirResolvedQualifier ||
                element.qualifierSymbol?.classId?.let(::isAllowedClass) != false) &&
            (element !is FirResolvable || isAllowedReference(element))

    private fun isAllowedReference(resolvable: FirResolvable): Boolean {
      val symbol =
          when (val reference = resolvable.calleeReference) {
            is FirErrorNamedReference -> return false
            is FirResolvedNamedReference -> reference.resolvedSymbol
            else -> return true
          }
      when (symbol) {
        is FirValueParameterSymbol,
        is FirAnonymousFunctionSymbol,
        is FirTypeParameterSymbol -> return true
        is FirClassLikeSymbol<*> -> return isAllowedClass(symbol.classId)
        is FirPropertySymbol -> if (symbol.isLocal) return true
        is FirVariableSymbol<*> -> if (symbol.callableId?.classId == null) return true
        else -> {}
      }
      val callable = (symbol as? FirCallableSymbol<*>)?.unwrapFakeOverrides() ?: return false
      val callableId = callable.callableId ?: return false
      // `when`, `try`, `!!` and `?:` resolve through synthetic calls that generate no call.
      if (callableId.packageName == SYNTHETIC_PACKAGE) return true
      if (resolvable is FirCall) markInlinedLambdas(resolvable, callable)
      if (callable.origin.fromSource) return isOwn(callable, callable.origin)
      val classId =
          callableId.classId
              ?: return isLibraryDeclarationAllowed(callableId.asSingleFqName().asString(), true)
      return isAllowedClass(classId)
    }

    private fun isLowerable(element: FirElement): Boolean =
        when (element) {
          is FirAnonymousObject,
          is FirRegularClass,
          is FirTypeAlias,
          is FirCallableReferenceAccess -> false
          is FirAnonymousFunction -> element in inlinedLambdas
          is FirFunction -> element === function
          is FirAnnotation ->
              element.annotationTypeRef.coneTypeOrNull?.classId?.packageFqName?.asString()?.let {
                it == "kotlin" || it.startsWith("kotlin.")
              } == true
          is FirWhenExpression -> !isWhenOverEnum(element)
          is FirResolvable ->
              (element.calleeReference as? FirResolvedNamedReference)?.resolvedSymbol.let {
                it !is FirCallableSymbol<*> || isLowerableCallee(it.unwrapFakeOverrides())
              }
          else -> true
        }

    // Codegen reads a `$WhenMappings` class that it generates next to the caller.
    private fun isWhenOverEnum(expression: FirWhenExpression): Boolean {
      val subject = expression.subjectVariable?.returnTypeRef?.coneTypeOrNull ?: return false
      val kind = regularClass(subject.classId ?: return false)?.classKind
      return kind == null || kind == ClassKind.ENUM_CLASS
    }

    private fun isLowerableCallee(callable: FirCallableSymbol<*>): Boolean {
      val callableId = callable.callableId ?: return true
      if (
          callable is FirValueParameterSymbol || (callable as? FirPropertySymbol)?.isLocal == true
      ) {
        return true
      }
      if (!callable.origin.fromSource) {
        // `entries` of a library enum reads an `$EntriesMappings` class generated next to the
        // caller.
        return callableId.asSingleFqName() != ENUM_ENTRIES_FUNCTION &&
            !(callable is FirPropertySymbol && callableId.callableName.asString() == "entries")
      }
      // The ABI drops private declarations, and callers reach a protected member through a
      // synthetic accessor unless the inline function is itself protected.
      val status = (callable.fir as? FirMemberDeclaration)?.status as? FirResolvedDeclarationStatus
      if (status == null || status.effectiveVisibility.privateApi) return false
      if (
          status.visibility == Visibilities.Protected &&
              (function.status as? FirResolvedDeclarationStatus)?.visibility !=
                  Visibilities.Protected
      ) {
        return false
      }
      return when (callable) {
        is FirPropertySymbol ->
            !callable.fir.isConst &&
                callable.fir.getter?.isInline != true &&
                callable.fir.setter?.isInline != true
        is FirNamedFunctionSymbol -> !callable.fir.isInline || isKeepable(callable.fir)
        else -> true
      }
    }

    // Only a non-null function-type parameter that is not `vararg`, `noinline` or `crossinline` is
    // inlined; a lambda passed to any other parameter generates a class.
    private fun markInlinedLambdas(call: FirCall, callee: FirCallableSymbol<*>) {
      if ((callee as? FirNamedFunctionSymbol)?.fir?.isInline != true) return
      for ((argument, parameter) in call.resolvedArgumentMapping.orEmpty()) {
        if (parameter.isNoinline || parameter.isCrossinline || parameter.isVararg) continue
        val type = parameter.returnTypeRef.coneTypeOrNull
        val classId = (type as? ConeClassLikeType)?.lookupTag?.classId
        if (
            type == null ||
                type.isMarkedNullable ||
                classId?.packageFqName?.asString() != "kotlin" ||
                !classId.shortClassName.asString().startsWith("Function")
        ) {
          continue
        }
        (argument as? FirAnonymousFunctionExpression)?.let {
          inlinedLambdas += it.anonymousFunction
        }
      }
    }
  }

  private companion object {
    val SYNTHETIC_PACKAGE = FqName("_synthetic")
    val ENUM_ENTRIES_FUNCTION = FqName("kotlin.enums.enumEntries")
  }
}
