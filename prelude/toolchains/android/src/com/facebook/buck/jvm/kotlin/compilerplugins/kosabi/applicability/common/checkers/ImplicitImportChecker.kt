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
import org.jetbrains.kotlin.KtFakeSourceElementKind
import org.jetbrains.kotlin.KtNodeTypes.LAMBDA_EXPRESSION
import org.jetbrains.kotlin.com.intellij.psi.PsiElement
import org.jetbrains.kotlin.diagnostics.DiagnosticReporter
import org.jetbrains.kotlin.diagnostics.reportOn
import org.jetbrains.kotlin.fir.FirElement
import org.jetbrains.kotlin.fir.FirSession
import org.jetbrains.kotlin.fir.analysis.checkers.FirTypeRefSource
import org.jetbrains.kotlin.fir.analysis.checkers.MppCheckerKind
import org.jetbrains.kotlin.fir.analysis.checkers.context.CheckerContext
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.DeclarationCheckers
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.FirFileChecker
import org.jetbrains.kotlin.fir.analysis.checkers.toClassLikeSymbol
import org.jetbrains.kotlin.fir.declarations.FirAnonymousObject
import org.jetbrains.kotlin.fir.declarations.FirFile
import org.jetbrains.kotlin.fir.packageFqName
import org.jetbrains.kotlin.fir.resolve.firClassLike
import org.jetbrains.kotlin.fir.types.AbbreviatedTypeAttribute
import org.jetbrains.kotlin.fir.types.FirTypeRef
import org.jetbrains.kotlin.fir.types.coneType
import org.jetbrains.kotlin.fir.types.impl.FirResolvedTypeRefImpl
import org.jetbrains.kotlin.fir.visitors.FirVisitorVoid
import org.jetbrains.kotlin.js.descriptorUtils.getKotlinTypeFqName
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.psi.KtAnnotationEntry
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.kotlin.psi.KtImportList
import org.jetbrains.kotlin.psi.KtTypeReference
import org.jetbrains.kotlin.resolve.BindingContext
import org.jetbrains.kotlin.text
import org.jetbrains.kotlin.types.AbbreviatedType
import org.jetbrains.kotlin.types.KotlinType

class ImplicitImportChecker(private val bindingContext: BindingContext) : TypeChecker() {

  override fun visitImportList(importList: KtImportList) {
    importNames =
        importList.imports
            .flatMap {
              it.importPath
                  ?.pathStr
                  ?.let { pathStr -> setOfNotNull(pathStr, pathStr.replace("`", "")) }
                  .orEmpty()
            }
            .toSet()
    super.visitImportList(importList)
  }

  override fun getViolationsOfType(type: KtTypeReference): List<String> {
    val kotlinType: KotlinType = bindingContext[BindingContext.TYPE, type] ?: return emptyList()
    val fqName = kotlinType.getKotlinTypeFqName(false)
    // skip closures
    if (type.isClosure(fqName)) {
      return emptyList()
    }

    val violatingKotlinTypes =
        getFilteredTypes(kotlinType).filterNot { filteredType ->
          when (filteredType) {
                is AbbreviatedType ->
                    if (filteredType.isViolatingTypeAlias()) filteredType
                    else filteredType.abbreviation
                else -> filteredType
              }
              .getKotlinTypeFqName(false)
              .isExplicitlyImported(type, pkg)
        }

    return violatingKotlinTypes.map { it.getKotlinTypeFqName(false) }
  }

  override fun visitAnnotationEntry(annotationEntry: KtAnnotationEntry) = Unit

  companion object : FeatureChecker {
    private val SDK_IMPORTS = listOf("java.", "kotlin.")
    internal var importNames: Set<String> = setOf()
    override val name: String = "Implicitly imported types"
    override val description: String =
        "This module has implicitly imported types. Non-SDK types that are used without import declaration."
    override val fix: CheckerFix =
        CheckerFix(
            CheckerFixType.MANUAL,
            // TODO(T125647307): Add examples of implicit imports
            """
                You should add explicit imports for such types.
                We found implicit imports in the following files:
                """
                .trimIndent())

    override val declarationCheckers: DeclarationCheckers =
        object : DeclarationCheckers() {
          override val fileCheckers: Set<FirFileChecker> = setOf(ImplicitImportFileChecker())
        }

    override fun ktFileChecker(file: KtFile, bindingContext: BindingContext): KtCheckerBase =
        ImplicitImportChecker(bindingContext)

    private fun buildImports(fqNames: List<FqName>): Set<String> {
      return fqNames.map { it.asString() }.toSet()
    }

    private fun FirTypeRef.isClosure(session: FirSession): Boolean {
      val classId = this.toClassLikeSymbol(session)?.classId ?: return false
      return classId.asFqNameString().isClosure()
    }

    private fun String.isClosure(): Boolean {
      return this.startsWith("kotlin.coroutines") || this.startsWith("kotlin.Function")
    }

    private fun PsiElement.isClosure(fqName: String): Boolean {
      if (this.getUserType() != null) {
        return false
      }
      return fqName.isClosure()
    }

    // K2 implementation
    private fun FirTypeRef.isExplicitlyImported(
        usedPackageName: FqName,
        session: FirSession,
        sourceText: String,
    ): Boolean {
      // lambda expressions can have parameters with not imported types
      if ((this as? FirResolvedTypeRefImpl)?.source?.treeStructure?.root?.tokenType ==
          LAMBDA_EXPRESSION) {
        return true
      }

      // If the type is a typealias, it should already be validated either by TypeAliasChecker or
      // by its local declaration in this file.
      if (this.coneType.attributes.contains(AbbreviatedTypeAttribute::class)) {
        return true
      }

      val classId = this.toClassLikeSymbol(session)?.classId ?: return true
      val classLike = this.firClassLike(session)
      // k2 visits anonymous objects as type references on their declarations
      // for example, when we create a case inside enum, the compiler creates a new anonymous object
      // dedicated for this case and visits the object as type reference on its declaration
      if (classLike is FirAnonymousObject) {
        return true
      }

      val fqName = classId.asFqNameString()
      return fqName.isExplicitlyImported(
          sourceText, usedPackageName.asString(), classId.packageFqName.asString())
    }

    // K1 implementation
    private fun String.isExplicitlyImported(
        typeReference: PsiElement?,
        packageFqName: String,
    ): Boolean {
      val userType = typeReference?.getUserType() ?: return false
      return this.isExplicitlyImported(userType.text, packageFqName, getPackageName(this))
    }

    private fun String.isExplicitlyImported(
        sourceText: String,
        usedPackageName: String,
        declaredPackageName: String
    ): Boolean {
      // Logical OR has short-circuit evaluation
      return isInImportBlock(this) ||
          isSdkImport(this) ||
          (usedPackageName == declaredPackageName) ||
          isFqNameDeclaredInType(this, sourceText) ||
          isImportedViaOuter(this, sourceText)
    }

    // The type is imported in [KtFile]
    private fun isInImportBlock(jetType: String): Boolean {
      return importNames.contains(jetType)
    }

    // This type is from SDK
    private fun isSdkImport(jetType: String): Boolean {
      return SDK_IMPORTS.any { jetType.startsWith(it) }
    }

    // The type is imported in [KtFile] via it's outer type
    // Example:
    // import com.a.b.c.X
    // T = X.Y
    // TODO(T125741437): This check is likely slow. Need an optimisation
    private fun isImportedViaOuter(jetType: String, sourceText: String): Boolean {
      val import = findRelatedImportIfAny(jetType) ?: return false
      val expectedUserType = import.split('.').last() + jetType.removePrefix(import)
      return expectedUserType in sourceText
    }

    private fun findRelatedImportIfAny(jetType: String): String? {
      var import: String? = null
      importNames.forEach {
        if (jetType.startsWith(it)) {
          if (it.length > import.orEmpty().length) {
            import = it
          }
        }
      }
      return import
    }

    // The type is declared with FqName expression in return type and match the jet type
    // val value: com.user.define.Type = ...
    private fun isFqNameDeclaredInType(jetType: String, sourceText: String): Boolean {
      return sourceText.contains(jetType)
    }

    class FileTypeRefsVisitor(private val file: FirFile, private val session: FirSession) :
        FirVisitorVoid() {

      private val _typeRefSourceSetWithoutExplicitImport: MutableSet<FirTypeRefSource> =
          mutableSetOf()
      val typeRefSourceSetWithoutExplicitImport: Set<FirTypeRefSource>
        get() = _typeRefSourceSetWithoutExplicitImport

      override fun visitElement(element: FirElement) {
        // ignore blocks
        if (element.isIrrelevantForAbi()) {
          return
        }

        (element as? FirTypeRef)?.let { visitTypeRef(it) }
        element.acceptChildren(this)
      }

      override fun visitTypeRef(typeRef: FirTypeRef) {
        // skip closures
        if (typeRef.isClosure(session)) {
          return
        }

        val packageName = file.packageFqName
        // we can not check a reference if we can't access source code
        val sourceText = typeRef.source?.text?.toString() ?: return

        typeRef.forEachFirTypeRefSource { typeRefSource: FirTypeRefSource ->
          if (typeRefSource.typeRef?.isExplicitlyImported(packageName, session, sourceText) ==
              false && typeRefSource.isNotImplicitTypeArgument()) {
            _typeRefSourceSetWithoutExplicitImport.add(typeRefSource)
          }
        }
      }

      private fun FirTypeRefSource.isNotImplicitTypeArgument() =
          this.source?.kind !is KtFakeSourceElementKind.ImplicitTypeArgument
    }

    class ImplicitImportFileChecker : FirFileChecker(MppCheckerKind.Common) {
      override fun check(
          declaration: FirFile,
          context: CheckerContext,
          reporter: DiagnosticReporter
      ) {
        importNames = buildImports(declaration.imports.mapNotNull { it.importedFqName })
        val visitor = FileTypeRefsVisitor(declaration, context.session)
        declaration.acceptChildren(visitor)

        for (typeRefSource in visitor.typeRefSourceSetWithoutExplicitImport) {
          val typeRef = typeRefSource.typeRef ?: continue

          reporter.reportOn(
              typeRefSource.source ?: declaration.source,
              FirApplicabilityErrors.IMPLICIT_IMPORT,
              typeRef.coneType,
              context)
        }
      }
    }
  }
}
