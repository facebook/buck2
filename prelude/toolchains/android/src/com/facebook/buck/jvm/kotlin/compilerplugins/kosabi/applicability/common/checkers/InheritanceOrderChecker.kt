/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.kotlin.compilerplugins.kosabi.applicability.common.checkers

import com.facebook.kotlin.compilerplugins.kosabi.applicability.common.CheckerFix
import com.facebook.kotlin.compilerplugins.kosabi.applicability.common.CheckerFixType
import com.facebook.kotlin.compilerplugins.kosabi.applicability.common.checkers.errors.FirApplicabilityErrors
import org.jetbrains.kotlin.descriptors.ClassKind
import org.jetbrains.kotlin.diagnostics.DiagnosticReporter
import org.jetbrains.kotlin.diagnostics.findChildByType
import org.jetbrains.kotlin.diagnostics.reportOn
import org.jetbrains.kotlin.fir.FirSession
import org.jetbrains.kotlin.fir.analysis.checkers.MppCheckerKind
import org.jetbrains.kotlin.fir.analysis.checkers.context.CheckerContext
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.DeclarationCheckers
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.FirClassChecker
import org.jetbrains.kotlin.fir.analysis.checkers.toRegularClassSymbol
import org.jetbrains.kotlin.fir.declarations.FirClass
import org.jetbrains.kotlin.fir.declarations.constructors
import org.jetbrains.kotlin.fir.declarations.utils.effectiveVisibility
import org.jetbrains.kotlin.fir.declarations.utils.isEnumClass
import org.jetbrains.kotlin.fir.resolve.toSymbol
import org.jetbrains.kotlin.fir.symbols.impl.FirClassSymbol
import org.jetbrains.kotlin.fir.symbols.impl.FirTypeAliasSymbol
import org.jetbrains.kotlin.fir.types.ConeClassLikeType
import org.jetbrains.kotlin.fir.types.FirTypeRef
import org.jetbrains.kotlin.fir.types.coneType
import org.jetbrains.kotlin.psi.KtBlockExpression
import org.jetbrains.kotlin.psi.KtClass
import org.jetbrains.kotlin.psi.KtConstructorCalleeExpression
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.kotlin.psi.psiUtil.isPrivate
import org.jetbrains.kotlin.psi.stubs.elements.KtStubElementTypes
import org.jetbrains.kotlin.resolve.BindingContext
import org.jetbrains.kotlin.types.KotlinType
import org.jetbrains.kotlin.types.typeUtil.isInterface

/*
  Checks if inheritance from a super class with no inplace ctor call is not first in
  the super type list.

  Why?
  Stubs generation can not distinguish between a super class or interface if super class doesn't
  have an inplace ctor call so having the super class as the first in the list can solve
  this issue
*/
class InheritanceOrderChecker(private val bindingContext: BindingContext) : KtCheckerBase() {
  override val violations: MutableList<Violation> = mutableListOf()
  private var hasSecDefaultCtor = false

  override fun visitClass(klass: KtClass) {
    if (klass.isPrivate()) {
      return
    }

    hasSecDefaultCtor =
        !klass.hasPrimaryConstructor() &&
            klass.secondaryConstructors.any { it.valueParameters.isEmpty() }

    val superTypeList = klass.getSuperTypeList()
    val superTypeEntries = klass.getSuperTypeList()?.entries
    if (superTypeList != null && !superTypeEntries.isNullOrEmpty()) {
      val index =
          superTypeEntries.indexOfFirst {
            isClass(bindingContext[BindingContext.TYPE, it.typeReference])
          }
      if (index == 0) {
        // super class is first in list
        return
      }
      if (index < 0) {
        // only interfaces exist
        if (hasSecDefaultCtor) {
          violations.add(
              Violation(
                  superTypeList.textOffset, "no super class --> must have primary constructor"))
        }
        return
      }
      if (superTypeEntries[index]
          .children
          .filter { it is KtConstructorCalleeExpression }
          .isEmpty()) {
        // has no inplace ctor
        violations.add(
            Violation(superTypeEntries[index].textOffset, "super class must be first in list"))
      }
    }

    super.visitClass(klass)
  }

  override fun visitBlockExpression(expression: KtBlockExpression) = Unit

  private fun isClass(type: KotlinType?): Boolean {
    if (type == null) {
      return false
    }
    return !type.isInterface()
  }

  companion object : FeatureChecker {
    override val name: String = "Inheritance"
    override val description: String =
        "This module has classes/objects that implement interfaces ambiguous to kosabi "
    override val fix: CheckerFix =
        CheckerFix(
            CheckerFixType.MANUAL,
            """
        classes/objects that implement interfaces may cause confusions related to super classes:
        - if it extends a super class --> need to have it first in list
            Example:
              class A: SuperClass(), Interface1, Interface2
        - if it only implements interfaces --> need to use primary constructor
            Example:
              class B(): Interface
        We found this issue in the following files:
      """
                .trimIndent())

    override val declarationCheckers: DeclarationCheckers =
        object : DeclarationCheckers() {
          override val classCheckers: Set<FirClassChecker> = setOf(InheritanceOrderCheckerK2())
        }

    override fun ktFileChecker(file: KtFile, bindingContext: BindingContext): KtCheckerBase =
        InheritanceOrderChecker(bindingContext)
  }
}

fun FirTypeRef.isNonInterface(context: CheckerContext): Boolean {
  val lookupTag = (this.coneType as? ConeClassLikeType)?.lookupTag ?: return false

  val tagSymbol = lookupTag.toSymbol(context.session) ?: return false

  if (tagSymbol is FirTypeAliasSymbol) {
    return tagSymbol.resolvedExpandedTypeRef.isNonInterface(context)
  }

  val symbol = tagSymbol as? FirClassSymbol<*> ?: return false

  return symbol.classKind != ClassKind.INTERFACE
}

// copied from upstream, but fixed it, because the upstream implementation does not deal well with
// type aliases
fun FirClass.findNonInterfaceSupertype(context: CheckerContext): FirTypeRef? {
  for (superTypeRef in superTypeRefs) {
    if (superTypeRef.isNonInterface(context)) {
      return superTypeRef
    }
  }
  return null
}

private class InheritanceOrderCheckerK2 : FirClassChecker(MppCheckerKind.Common) {
  override fun check(declaration: FirClass, context: CheckerContext, reporter: DiagnosticReporter) {
    // enums are final and can't inherit from any classes except
    // Kotlin/Enum, which is always inherited implicitly
    if (declaration.isEnumClass) {
      return
    }
    val superTypes =
        declaration.superTypeRefs
            .mapNotNull { it.toRegularClassSymbol(context.session)?.classId?.asFqNameString() }
            .filterNot { it == "kotlin.Any" }

    // We only care about objects/class with supertypes that are
    // public, top-level, and are not in blocks
    if (superTypes.isEmpty() ||
        declaration.effectiveVisibility.privateApi ||
        declaration.source.parentIsBlock()
    // TODO: check if declarations is top level. before it was like:
    // (declaration.source.psi as? KtClassOrObject)?.isTopLevel() == false
    ) {
      return
    }

    val superClassFirTypeRef = declaration.findNonInterfaceSupertype(context)

    val superClass =
        superClassFirTypeRef?.toRegularClassSymbol(context.session)?.classId?.asFqNameString()

    val indexOfSuperClass = superTypes.indexOf(superClass)

    when {
      // if super class is first in list, we pass
      indexOfSuperClass == 0 -> {
        return
      }
      // if we have a super class that is not first in list, we must have in place ctor
      indexOfSuperClass > 0 -> {
        requireNotNull(superClassFirTypeRef)

        val superTypeList =
            declaration.source?.lighterASTNode?.let {
              declaration.source
                  ?.treeStructure
                  ?.findChildByType(it, KtStubElementTypes.SUPER_TYPE_LIST)
            }

        val superTypeConstructorCall =
            superTypeList?.let {
              declaration.source
                  ?.treeStructure
                  ?.findChildByType(superTypeList, KtStubElementTypes.SUPER_TYPE_CALL_ENTRY)
            }

        if (superTypeConstructorCall == null) {
          reporter.reportOn(
              superClassFirTypeRef.source,
              FirApplicabilityErrors.INHERITANCE_ORDER_SUPERCLASS_FIRST,
              superClassFirTypeRef.coneType,
              context)
        }
      }
      // if we only have interfaces, there must be a primary constructor
      indexOfSuperClass < 0 -> {
        if (declaration.hasEligibleConstructor(context.session)) {
          reporter.reportOn(
              declaration.source,
              FirApplicabilityErrors.INHERITANCE_ORDER_NO_PRIMARY_CONSTRUCTOR,
              declaration.symbol,
              context)
        }
      }
    }
  }

  private fun FirClass.hasEligibleConstructor(session: FirSession): Boolean {
    return this.constructors(session).isNotEmpty() &&
        this.constructors(session).all { !it.isPrimary } &&
        this.constructors(session).any { it.valueParameterSymbols.isEmpty() }
  }
}
