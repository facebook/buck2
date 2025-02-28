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
import com.facebook.kotlin.compilerplugins.kosabi.applicability.common.CheckerFixType.MANUAL
import com.facebook.kotlin.compilerplugins.kosabi.applicability.common.checkers.errors.FirApplicabilityErrors
import org.jetbrains.kotlin.diagnostics.DiagnosticReporter
import org.jetbrains.kotlin.diagnostics.reportOn
import org.jetbrains.kotlin.fir.analysis.checkers.MppCheckerKind
import org.jetbrains.kotlin.fir.analysis.checkers.context.CheckerContext
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.DeclarationCheckers
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.FirClassChecker
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.FirClassLikeChecker
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.FirFunctionChecker
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.FirPropertyChecker
import org.jetbrains.kotlin.fir.analysis.checkers.hasModifier
import org.jetbrains.kotlin.fir.declarations.FirClassLikeDeclaration
import org.jetbrains.kotlin.fir.declarations.FirFunction
import org.jetbrains.kotlin.fir.declarations.FirMemberDeclaration
import org.jetbrains.kotlin.fir.declarations.FirProperty
import org.jetbrains.kotlin.fir.declarations.utils.effectiveVisibility
import org.jetbrains.kotlin.lexer.KtTokens.INLINE_KEYWORD
import org.jetbrains.kotlin.psi.KtClassOrObject
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.kotlin.psi.KtModifierListOwner
import org.jetbrains.kotlin.psi.KtNamedFunction
import org.jetbrains.kotlin.psi.KtProperty
import org.jetbrains.kotlin.psi.psiUtil.isPrivate
import org.jetbrains.kotlin.resolve.BindingContext

class InlineChecker : KtCheckerBase() {
  override val violations: MutableList<Violation> = mutableListOf()

  override fun visitNamedFunction(function: KtNamedFunction) {
    if (function.hasModifier(INLINE_KEYWORD) && !function.isPrivate()) {
      val offset = getInlineKeywordOffset(function) ?: function.textOffset
      violations.add(Violation(offset))
    }
    super.visitNamedFunction(function)
  }

  override fun visitProperty(property: KtProperty) {
    if (property.hasModifier(INLINE_KEYWORD) && !property.isPrivate()) {
      val offset = getInlineKeywordOffset(property) ?: property.textOffset
      violations.add(Violation(offset))
    }
    super.visitProperty(property)
  }

  override fun visitClassOrObject(classOrObject: KtClassOrObject) {
    if (classOrObject.hasModifier(INLINE_KEYWORD) && !classOrObject.isPrivate()) {
      val offset = getInlineKeywordOffset(classOrObject) ?: classOrObject.textOffset
      violations.add(Violation(offset))
    }
    super.visitClassOrObject(classOrObject)
  }

  private fun getInlineKeywordOffset(element: KtModifierListOwner): Int? {
    return element.modifierList?.getModifier(INLINE_KEYWORD)?.textOffset
  }

  companion object : FeatureChecker {
    override val name: String = "Inline keyword"
    override val description: String = "This module has non-private inline functions."
    override val fix: CheckerFix =
        CheckerFix(MANUAL, "We found non-private inline keyword in the following files:")

    override val declarationCheckers: DeclarationCheckers =
        object : DeclarationCheckers() {
          override val classCheckers: Set<FirClassChecker> = setOf(InlineClassCheckerK2())
          override val functionCheckers: Set<FirFunctionChecker> = setOf(InlineFunctionCheckerK2())
          override val propertyCheckers: Set<FirPropertyChecker> = setOf(InlinePropertyCheckerK2())
        }

    override fun ktFileChecker(file: KtFile, bindingContext: BindingContext): KtCheckerBase =
        InlineChecker()
  }
}

private class InlineClassCheckerK2 : FirClassLikeChecker(MppCheckerKind.Common) {
  override fun check(
      declaration: FirClassLikeDeclaration,
      context: CheckerContext,
      reporter: DiagnosticReporter,
  ) = declaration.checkInline(context, reporter)
}

private class InlineFunctionCheckerK2 : FirFunctionChecker(MppCheckerKind.Common) {
  override fun check(
      declaration: FirFunction,
      context: CheckerContext,
      reporter: DiagnosticReporter,
  ) = declaration.checkInline(context, reporter)
}

private class InlinePropertyCheckerK2 : FirPropertyChecker(MppCheckerKind.Common) {
  override fun check(
      declaration: FirProperty,
      context: CheckerContext,
      reporter: DiagnosticReporter,
  ) = declaration.checkInline(context, reporter)
}

private fun FirMemberDeclaration.checkInline(
    context: CheckerContext,
    reporter: DiagnosticReporter,
) {
  if (hasModifier(INLINE_KEYWORD) && !effectiveVisibility.privateApi) {
    reporter.reportOn(source, FirApplicabilityErrors.INLINE_KEYWORD, symbol, context)
  }
}
