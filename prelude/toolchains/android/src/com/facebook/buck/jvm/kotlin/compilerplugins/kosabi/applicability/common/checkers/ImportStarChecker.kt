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
import org.jetbrains.kotlin.fir.analysis.checkers.context.CheckerContext
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.DeclarationCheckers
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.FirFileChecker
import org.jetbrains.kotlin.fir.declarations.FirFile
import org.jetbrains.kotlin.fir.declarations.FirImport
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.kotlin.psi.KtImportDirective
import org.jetbrains.kotlin.resolve.BindingContext

class ImportStarChecker : KtCheckerBase() {
  override val violations: MutableList<Violation> = mutableListOf()

  override fun visitImportDirective(importDirective: KtImportDirective) {
    if (importDirective.importedName == null) {
      violations.add(Violation(importDirective.textOffset))
    }
    return super.visitImportDirective(importDirective)
  }

  companion object : FeatureChecker {
    override val name: String = "Star imports"
    override val description: String = "This module has star imports."
    override val fix: CheckerFix =
        CheckerFix(
            CheckerFixType.MANUAL,
            """
        You should remove star imports.
        We found star imports in the following files:
      """
                .trimIndent())

    override val declarationCheckers: DeclarationCheckers =
        object : DeclarationCheckers() {
          override val fileCheckers: Set<FirFileChecker> = setOf(ImportStarCheckerK2())
        }

    override fun ktFileChecker(file: KtFile, bindingContext: BindingContext): KtCheckerBase =
        ImportStarChecker()
  }
}

private class ImportStarCheckerK2 : FirFileChecker(MppCheckerKind.Common) {
  override fun check(declaration: FirFile, context: CheckerContext, reporter: DiagnosticReporter) {
    for (imp in declaration.imports) {
      if (imp.isAllUnder) {
        reporter.reportOn(
            imp.source ?: declaration.source,
            FirApplicabilityErrors.STAR_IMPORT,
            imp.requireImportedStarNameAsString(),
            context)
      }
    }
  }

  private fun FirImport.requireImportedStarNameAsString(): String =
      "${requireNotNull(importedFqName?.asString())}.*"
}
