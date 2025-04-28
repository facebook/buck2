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
import org.jetbrains.kotlin.AbstractKtSourceElement
import org.jetbrains.kotlin.com.intellij.psi.PsiElement
import org.jetbrains.kotlin.config.CompilerConfiguration
import org.jetbrains.kotlin.diagnostics.DiagnosticContext
import org.jetbrains.kotlin.diagnostics.DiagnosticReporter
import org.jetbrains.kotlin.diagnostics.KtDiagnosticFactory2
import org.jetbrains.kotlin.diagnostics.error2
import org.jetbrains.kotlin.diagnostics.reportOn
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.DeclarationCheckers
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.kotlin.resolve.BindingContext

interface Checker {
  val name: String
  val description: String
  val fix: CheckerFix
}

interface ProjectChecker : Checker {
  fun findViolations(configuration: CompilerConfiguration): List<String>
}

interface FeatureChecker : Checker {
  val declarationCheckers: DeclarationCheckers

  fun ktFileChecker(file: KtFile, bindingContext: BindingContext): KtCheckerBase

  fun findViolations(file: KtFile, bindingContext: BindingContext): List<Violation> =
      ktFileChecker(file, bindingContext).apply { visitKtElement(file) }.violations
}

fun DiagnosticReporter.reportOn(
    source: AbstractKtSourceElement?,
    checker: FeatureChecker,
    context: DiagnosticContext,
) = reportOn(source, ERROR_FACTORY, checker.name, checker.description, context)

private val ERROR_FACTORY: KtDiagnosticFactory2<String, String> by
    error2<PsiElement, String, String>()
