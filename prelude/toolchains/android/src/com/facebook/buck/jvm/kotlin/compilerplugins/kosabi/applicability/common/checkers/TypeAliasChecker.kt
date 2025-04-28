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
import com.facebook.kotlin.compilerplugins.kosabi.applicability.common.checkers.TypeChecker.Companion.pkg
import com.facebook.kotlin.compilerplugins.kosabi.applicability.common.checkers.errors.FirApplicabilityErrors.TYPE_ALIAS
import org.jetbrains.kotlin.diagnostics.DiagnosticReporter
import org.jetbrains.kotlin.diagnostics.reportOn
import org.jetbrains.kotlin.fir.FirElement
import org.jetbrains.kotlin.fir.FirPackageDirective
import org.jetbrains.kotlin.fir.FirSession
import org.jetbrains.kotlin.fir.analysis.checkers.MppCheckerKind
import org.jetbrains.kotlin.fir.analysis.checkers.context.CheckerContext
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.DeclarationCheckers
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.FirFileChecker
import org.jetbrains.kotlin.fir.declarations.FirFile
import org.jetbrains.kotlin.fir.declarations.utils.classId
import org.jetbrains.kotlin.fir.expressions.FirAnnotationCall
import org.jetbrains.kotlin.fir.types.AbbreviatedTypeAttribute
import org.jetbrains.kotlin.fir.types.ConeClassLikeType
import org.jetbrains.kotlin.fir.types.FirTypeRef
import org.jetbrains.kotlin.fir.types.coneType
import org.jetbrains.kotlin.fir.visitors.FirVisitor
import org.jetbrains.kotlin.js.descriptorUtils.getJetTypeFqName
import org.jetbrains.kotlin.psi.KtAnnotationEntry
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.kotlin.psi.KtTypeReference
import org.jetbrains.kotlin.resolve.BindingContext
import org.jetbrains.kotlin.types.KotlinType
import org.jetbrains.kotlin.types.getAbbreviation
import org.jetbrains.kotlin.types.typeUtil.containsTypeAliases

class TypeAliasChecker(private val bindingContext: BindingContext) : TypeChecker() {

  override fun getViolationsOfType(type: KtTypeReference): List<String> {
    val kotlinType = bindingContext[BindingContext.TYPE, type] ?: return emptyList()
    val violatingKotlinTypes = getFilteredTypes(kotlinType).filter { it.isViolatingTypeAlias() }
    return violatingKotlinTypes.map { it.getAbbreviation().toString() }
  }

  override fun visitAnnotationEntry(annotationEntry: KtAnnotationEntry) = Unit

  companion object : FeatureChecker {
    override val name: String = "Typealias declarations"
    override val description: String =
        "This module uses typealias type declarations in non-private methods."
    override val fix: CheckerFix =
        CheckerFix(CheckerFixType.MANUAL, "We found typealias usage in the following files:")

    override val declarationCheckers: DeclarationCheckers
      get() =
          object : DeclarationCheckers() {
            override val fileCheckers: Set<FirFileChecker>
              get() = setOf(TypeAliasFileChecker())
          }

    override fun ktFileChecker(file: KtFile, bindingContext: BindingContext): KtCheckerBase =
        TypeAliasChecker(bindingContext)

    class FirFileVisitor(private val session: FirSession) :
        FirVisitor<MutableList<FirTypeRef>, MutableList<FirTypeRef>>() {
      var packageName: String = ""

      override fun visitElement(
          element: FirElement,
          data: MutableList<FirTypeRef>
      ): MutableList<FirTypeRef> {
        if (element is FirAnnotationCall) {
          return data
        }

        if (element.isIrrelevantForAbi()) {
          return data
        }

        (element as? FirTypeRef)?.let { visitTypeRef(it, data) }
        (element as? FirPackageDirective)?.let { visitPackageDirective(it, data) }
        element.acceptChildren(this, data)
        return data
      }

      override fun visitTypeRef(
          typeRef: FirTypeRef,
          data: MutableList<FirTypeRef>
      ): MutableList<FirTypeRef> {
        for (innerType in typeRef.constituentTypes()) {
          val alias =
              innerType.coneType.attributes[AbbreviatedTypeAttribute::class]
                  ?.let { it.coneType as? ConeClassLikeType }
                  ?.lookupTag
                  ?.classId ?: continue
          if (!alias.asFqNameString().isSdkTypeAlias() &&
              alias.packageFqName.asString() != packageName) {
            data.add(typeRef)
            break
          }
        }

        return data
      }

      override fun visitPackageDirective(
          packageDirective: FirPackageDirective,
          data: MutableList<FirTypeRef>
      ): MutableList<FirTypeRef> {
        packageName = packageDirective.packageFqName.asString()
        return data
      }
    }

    class TypeAliasFileChecker : FirFileChecker(MppCheckerKind.Common) {
      override fun check(
          declaration: FirFile,
          context: CheckerContext,
          reporter: DiagnosticReporter,
      ) {
        val visitor = FirFileVisitor(context.session)
        val violations = declaration.accept(visitor, mutableListOf())
        violations.forEach { type ->
          reporter.reportOn(type.source, TYPE_ALIAS, type.coneType, context)
        }
      }
    }
  }
}

internal fun KotlinType.isViolatingTypeAlias(): Boolean {
  val abbr = this.getAbbreviation() ?: return false
  // Allow type aliases for SDK classes
  val jetType = abbr.getJetTypeFqName(false)
  if (jetType.isSdkTypeAlias()) {
    return false
  }

  if (isDeclaredLocally(jetType, pkg)) {
    return false
  }

  return abbr.containsTypeAliases()
}

private fun String?.isSdkTypeAlias(): Boolean {
  if (this == null) {
    return false
  }
  return startsWith("kotlin.") || startsWith("kotlinx.") || startsWith("java.")
}
