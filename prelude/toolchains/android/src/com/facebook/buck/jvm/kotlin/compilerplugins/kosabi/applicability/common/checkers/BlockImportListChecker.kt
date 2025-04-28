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
import com.facebook.kotlin.compilerplugins.kosabi.applicability.common.config.Config.blockImportList
import com.facebook.kotlin.compilerplugins.kosabi.applicability.common.config.Config.blockImportPrefixList
import org.jetbrains.kotlin.diagnostics.DiagnosticReporter
import org.jetbrains.kotlin.diagnostics.reportOn
import org.jetbrains.kotlin.fir.analysis.checkers.MppCheckerKind
import org.jetbrains.kotlin.fir.analysis.checkers.context.CheckerContext
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.DeclarationCheckers
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.FirFileChecker
import org.jetbrains.kotlin.fir.declarations.FirFile
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.kotlin.psi.KtImportDirective
import org.jetbrains.kotlin.resolve.BindingContext

class BlockImportListChecker : KtCheckerBase() {
  override val violations: MutableList<Violation> = mutableListOf()

  override fun visitImportDirective(importDirective: KtImportDirective) {
    val importPath = importDirective.importPath
    if (importPath != null && importPath.fqName.isBlockedImport()) {
      violations.add(Violation(importDirective.textOffset, importPath.pathStr))
    }
    return super.visitImportDirective(importDirective)
  }

  companion object : FeatureChecker {
    override val name: String = "Blocked imports"
    override val description: String = "This module contains imports from BlockList."
    override val fix: CheckerFix =
        CheckerFix(
            CheckerFixType.MANUAL,
            """
        You should remove blocked imports.
        See all blocked imports -- [com.facebook.kotlin.compilerplugins.kosabi.applicability.config.Config.kt]
        We found blocked imports in the following files:
      """
                .trimIndent())

    override val declarationCheckers: DeclarationCheckers =
        object : DeclarationCheckers() {
          override val fileCheckers: Set<FirFileChecker> = setOf(BlockImportListCheckerK2())
        }

    override fun ktFileChecker(file: KtFile, bindingContext: BindingContext): KtCheckerBase =
        BlockImportListChecker()
  }
}

private class BlockImportListCheckerK2 : FirFileChecker(MppCheckerKind.Common) {
  override fun check(declaration: FirFile, context: CheckerContext, reporter: DiagnosticReporter) {
    for (imp in declaration.imports) {
      val importedFqName = imp.importedFqName
      if (importedFqName?.isBlockedImport() == true) {
        reporter.reportOn(
            imp.source ?: declaration.source,
            FirApplicabilityErrors.BLOCKED_IMPORT,
            importedFqName.asString(),
            context)
      }
    }
  }
}

private fun FqName.isBlockedImport(): Boolean {
  val nameString = toString()
  return blockImportList.contains(nameString) || isBlockedPrefix(nameString)
}

private fun isBlockedPrefix(string: String): Boolean {
  return blockImportPrefixList.any { string.startsWith(it) }
}
