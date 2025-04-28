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
import org.jetbrains.kotlin.com.intellij.psi.PsiElement
import org.jetbrains.kotlin.diagnostics.DiagnosticReporter
import org.jetbrains.kotlin.diagnostics.reportOn
import org.jetbrains.kotlin.fir.FirElement
import org.jetbrains.kotlin.fir.analysis.checkers.MppCheckerKind
import org.jetbrains.kotlin.fir.analysis.checkers.context.CheckerContext
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.DeclarationCheckers
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.FirFileChecker
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.FirPropertyChecker
import org.jetbrains.kotlin.fir.declarations.FirFile
import org.jetbrains.kotlin.fir.declarations.FirProperty
import org.jetbrains.kotlin.fir.declarations.utils.isConst
import org.jetbrains.kotlin.fir.declarations.utils.visibility
import org.jetbrains.kotlin.fir.references.FirNamedReference
import org.jetbrains.kotlin.fir.references.toResolvedCallableSymbol
import org.jetbrains.kotlin.fir.visitors.FirVisitorVoid
import org.jetbrains.kotlin.lexer.KtTokens
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.psi.KtAnnotationEntry
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.kotlin.psi.KtImportList
import org.jetbrains.kotlin.psi.KtNameReferenceExpression
import org.jetbrains.kotlin.psi.KtProperty
import org.jetbrains.kotlin.psi.psiUtil.collectDescendantsOfType
import org.jetbrains.kotlin.psi.psiUtil.visibilityModifierTypeOrDefault
import org.jetbrains.kotlin.resolve.BindingContext
import org.jetbrains.kotlin.text

private const val SUPPRESS_LINT_TOKEN = "SuppressLint"
private const val KOSABI_IMPLICIT_CONST_VALUE_TOKEN = "KosabiApplicabilityImplicitConstantValue"
private const val KOSABI_IMPLICIT_CONST_VALUE_SUPRESSION_TOKEN =
    "@SuppressLint(\"KosabiApplicabilityImplicitConstantValue\")"

class ImplicitConstantValueChecker : KtCheckerBase() {
  override val violations: MutableList<Violation> = mutableListOf()

  override fun visitImportList(importList: KtImportList) {
    importNames = buildImportList(importList.imports.mapNotNull { it.importedFqName })
    super.visitImportList(importList)
  }

  override fun visitProperty(property: KtProperty) {
    if (!property.isLocal &&
        property.visibilityModifierTypeOrDefault() == KtTokens.PUBLIC_KEYWORD &&
        property.hasModifier(KtTokens.CONST_KEYWORD)) {

      if (importNames.intersect(property.toNonprimitiveChildren()).isNotEmpty() &&
          !property.annotationEntries.any {
            it.isAnnotationWithArg(SUPPRESS_LINT_TOKEN, KOSABI_IMPLICIT_CONST_VALUE_TOKEN)
          }) {
        violations.add(Violation(property.textOffset, property.annotations.toString()))
      }
    }
    super.visitProperty(property)
  }

  companion object : FeatureChecker {
    internal var importNames: Set<String> = setOf()

    override val name: String = "Implicit value assigned to const"
    override val description: String = "This module has implicit value set to const."
    override val fix: CheckerFix =
        CheckerFix(
            CheckerFixType.MANUAL,
            """
        You should add explicit primitive value for public constant. You could either make it private or remove const modifier for implicit usage.
        If the source const is defined in a module in required_for_source_only_abi list, you could add annotation @SuppressLint("KosabiApplicabilityImplicitConstantValue") to suppress this error
        We found implicit constant value set in the following files:
      """
                .trimIndent())

    override val declarationCheckers: DeclarationCheckers
      get() =
          object : DeclarationCheckers() {
            override val propertyCheckers: Set<FirPropertyChecker>
              get() = setOf(ImplicitConstantValueCheckerK2())

            override val fileCheckers: Set<FirFileChecker>
              get() = setOf(ImplicitConstantValueImportCheckerK2())
          }

    override fun ktFileChecker(file: KtFile, bindingContext: BindingContext): KtCheckerBase =
        ImplicitConstantValueChecker()

    private fun buildImportList(fqNames: List<FqName>): Set<String> =
        fqNames
            .filterNot { fgName -> fgName.startsWith(FqName("android")) }
            .map { it.shortName() }
            .map { it.identifier }
            .toSet()

    private fun PsiElement?.toNonprimitiveChildren(): Set<String> {
      return this?.lastChild
          ?.collectDescendantsOfType<KtNameReferenceExpression>()
          ?.map { it.getReferencedName() }
          ?.toSet()
          .orEmpty()
    }
  }

  private class NameReferenceVisitor(private val imports: Set<String>) : FirVisitorVoid() {

    override fun visitElement(element: FirElement) {
      if (hasImportedReferences) {
        return
      }
      (element as? FirNamedReference)?.let { visitNamedReference(it) }
      element.acceptChildren(this)
    }

    override fun visitNamedReference(namedReference: FirNamedReference) {
      val classId = namedReference.toResolvedCallableSymbol()?.callableId?.classId

      hasImportedReferences =
          hasImportedReferences ||
              importNames.contains(classId?.shortClassName?.asString()) ||
              importNames.contains(classId?.outermostClassId?.shortClassName?.toString()) ||
              importNames.contains(namedReference.name.asString())
    }

    var hasImportedReferences: Boolean = false
      private set
  }

  private class ImplicitConstantValueCheckerK2 : FirPropertyChecker(MppCheckerKind.Common) {

    val FirProperty.isCheckExplicitlySuppressed: Boolean
      get() = this.source.text?.contains(KOSABI_IMPLICIT_CONST_VALUE_SUPRESSION_TOKEN) == true

    override fun check(
        declaration: FirProperty,
        context: CheckerContext,
        reporter: DiagnosticReporter
    ) {
      if (!declaration.isLocal &&
          declaration.visibility.isPublicAPI &&
          declaration.isConst &&
          !declaration.isCheckExplicitlySuppressed) {
        val referencesVisitor = NameReferenceVisitor(importNames)
        declaration.initializer?.acceptChildren(referencesVisitor)
        if (referencesVisitor.hasImportedReferences) {
          reporter.reportOn(
              declaration.source,
              FirApplicabilityErrors.IMPLICIT_CONSTANT,
              declaration.symbol,
              context)
        }
      }
    }
  }

  private class ImplicitConstantValueImportCheckerK2 : FirFileChecker(MppCheckerKind.Common) {
    override fun check(
        declaration: FirFile,
        context: CheckerContext,
        reporter: DiagnosticReporter
    ) {
      importNames = buildImportList(declaration.imports.mapNotNull { it.importedFqName })
    }
  }

  fun KtAnnotationEntry.isAnnotationWithArg(annotation: String, arg: String): Boolean {
    return this?.shortName?.identifier == annotation &&
        this?.valueArguments?.firstOrNull()?.asElement()?.text?.contains(arg) == true
  }
}
