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
import org.jetbrains.kotlin.diagnostics.DiagnosticReporter
import org.jetbrains.kotlin.diagnostics.reportOn
import org.jetbrains.kotlin.fir.analysis.checkers.MppCheckerKind
import org.jetbrains.kotlin.fir.analysis.checkers.collectSupertypesWithDelegates
import org.jetbrains.kotlin.fir.analysis.checkers.context.CheckerContext
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.DeclarationCheckers
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.FirClassChecker
import org.jetbrains.kotlin.fir.analysis.checkers.toRegularClassSymbol
import org.jetbrains.kotlin.fir.declarations.FirClass
import org.jetbrains.kotlin.fir.declarations.utils.classId
import org.jetbrains.kotlin.fir.declarations.utils.visibility
import org.jetbrains.kotlin.fir.types.coneType
import org.jetbrains.kotlin.fir.types.renderReadableWithFqNames
import org.jetbrains.kotlin.js.descriptorUtils.getJetTypeFqName
import org.jetbrains.kotlin.psi.KtClass
import org.jetbrains.kotlin.psi.KtDelegatedSuperTypeEntry
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.kotlin.psi.KtPackageDirective
import org.jetbrains.kotlin.psi.psiUtil.isPublic
import org.jetbrains.kotlin.resolve.BindingContext
import org.jetbrains.kotlin.types.KotlinType

class SuperDelegationChecker(val bindingContext: BindingContext) : KtCheckerBase() {
  override val violations: MutableList<Violation> = mutableListOf()
  private lateinit var pkg: String

  override fun visitPackageDirective(directive: KtPackageDirective) {
    pkg = directive.qualifiedName
    super.visitPackageDirective(directive)
  }

  override fun visitClass(ktClass: KtClass) {
    if (!ktClass.isPublic) {
      return
    }
    super.visitClass(ktClass)
  }

  override fun visitDelegatedSuperTypeEntry(specifier: KtDelegatedSuperTypeEntry) {
    val kotlinType: KotlinType? = bindingContext[BindingContext.TYPE, specifier.typeReference]
    if (kotlinType != null) {
      val jetType = kotlinType.getJetTypeFqName(false)
      if (!isDeclaredLocally(jetType)) {
        violations.add(Violation(specifier.textOffset))
      }
    }
    super.visitDelegatedSuperTypeEntry(specifier)
  }

  private fun isDeclaredLocally(jetType: String): Boolean {
    val parentPkg =
        jetType
            .split('.')
            .takeWhile { it.isNotEmpty() && it.first().isLowerCase() }
            .joinToString(separator = ".")

    // The type is declared in [pkg] scope
    return parentPkg == pkg
  }

  companion object : FeatureChecker {
    override val name: String = "Super Delegation"
    override val description: String =
        "This module has classes/objects that uses 'by' keyword in Super List"
    override val fix: CheckerFix =
        CheckerFix(
            CheckerFixType.MANUAL,
            """
            Delegation in super list is not yet supported in kosabi
            Example:
              class A: B, C by D
        We found this issue in the following files:
      """
                .trimIndent())

    override val declarationCheckers: DeclarationCheckers
      get() =
          object : DeclarationCheckers() {
            override val classCheckers: Set<FirClassChecker>
              get() = setOf(SuperDelegationCheckerK2())
          }

    override fun ktFileChecker(file: KtFile, bindingContext: BindingContext): KtCheckerBase =
        SuperDelegationChecker(bindingContext)
  }

  class SuperDelegationCheckerK2 : FirClassChecker(MppCheckerKind.Common) {
    override fun check(
        declaration: FirClass,
        context: CheckerContext,
        reporter: DiagnosticReporter
    ) {
      if (declaration.visibility.isPublicAPI) {
        val parentPkg = declaration.classId.packageFqName.asString()

        declaration
            .collectSupertypesWithDelegates()
            .filter { (firType, firSymbol) ->
              firSymbol != null &&
                  parentPkg !=
                      firType
                          .toRegularClassSymbol(context.session)
                          ?.classId
                          ?.packageFqName
                          ?.asString()
            }
            .forEach { (firType, firSymbol) ->
              if (firSymbol != null) {
                reporter.reportOn(
                    firType.source,
                    FirApplicabilityErrors.SUPER_DELEGATION,
                    firSymbol.source?.getElementTextInContextForDebug()
                        ?: firType.coneType.renderReadableWithFqNames(),
                    firType.coneType,
                    context)
              }
            }
      }
    }
  }
}
