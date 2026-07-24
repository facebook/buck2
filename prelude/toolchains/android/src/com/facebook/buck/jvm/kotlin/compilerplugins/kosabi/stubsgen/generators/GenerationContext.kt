/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.kotlin.compilerplugins.kosabi.stubsgen.generators

import com.facebook.kotlin.compilerplugins.kosabi.common.FullTypeQualifier
import com.facebook.kotlin.compilerplugins.kosabi.common.parseClasspathFileClassesAndPackages
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.stub.container.StubsContainer
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.stub.container.StubsContainerImpl
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.util.calculateQualifierList
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.util.declaredTypes
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.util.toImportedClass
import java.io.File
import org.jetbrains.kotlin.com.intellij.psi.PsiElement
import org.jetbrains.kotlin.lexer.KtTokens
import org.jetbrains.kotlin.psi.KtAnnotationEntry
import org.jetbrains.kotlin.psi.KtCallExpression
import org.jetbrains.kotlin.psi.KtClass
import org.jetbrains.kotlin.psi.KtClassBody
import org.jetbrains.kotlin.psi.KtClassOrObject
import org.jetbrains.kotlin.psi.KtDelegatedSuperTypeEntry
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.kotlin.psi.KtNameReferenceExpression
import org.jetbrains.kotlin.psi.KtNullableType
import org.jetbrains.kotlin.psi.KtParameterList
import org.jetbrains.kotlin.psi.KtSecondaryConstructor
import org.jetbrains.kotlin.psi.KtSuperTypeCallEntry
import org.jetbrains.kotlin.psi.KtSuperTypeEntry
import org.jetbrains.kotlin.psi.KtSuperTypeList
import org.jetbrains.kotlin.psi.KtSuperTypeListEntry
import org.jetbrains.kotlin.psi.KtTreeVisitorVoid
import org.jetbrains.kotlin.psi.KtTypeAlias
import org.jetbrains.kotlin.psi.KtTypeParameter
import org.jetbrains.kotlin.psi.KtTypeReference
import org.jetbrains.kotlin.psi.KtUserType
import org.jetbrains.kotlin.psi.psiUtil.getChildOfType
import org.jetbrains.kotlin.psi.psiUtil.getChildrenOfType
import org.jetbrains.kotlin.psi.psiUtil.referenceExpression

class GenerationContext {
  val projectFiles: Collection<KtFile>
  val stubsContainer: StubsContainer
  val importedDeclarations: Set<FullTypeQualifier>
  val importedTypes: Set<FullTypeQualifier>

  /**
   * This contains external types outside from the local source codes, etc: classpath, code
   * generation
   */
  val externalTypeReferences: Set<FullTypeQualifier>
  val pkgsInClasspath: Set<List<String>>
  val interfaceTypes: List<KtUserType>
  val annotationEntries: List<KtAnnotationEntry>
  val declaredTypes: Set<FullTypeQualifier>
  val fullQualifierTypes: Set<FullTypeQualifier>
  val importAlias: Set<String>
  val typeAliasSymbol: Set<String>
  val parameterNames: Set<String>

  // Element: (type simple name -> num of ctor args)
  val typeValueArgs: Map<String, Int>
  val usedUserTypes: Set<KtUserType>

  constructor(
      projectFiles: Collection<KtFile>,
      classpath: List<File>,
      knownGeneratedTypes: Set<FullTypeQualifier> = emptySet(),
      lightweight: Boolean = true,
  ) {
    this.projectFiles = projectFiles
    this.stubsContainer = StubsContainerImpl()

    if (lightweight) {
      this.externalTypeReferences = emptySet()
      this.pkgsInClasspath = emptySet()
      this.importedDeclarations = emptySet()
      this.importedTypes = emptySet()
      this.interfaceTypes = emptyList()
      this.annotationEntries = emptyList()
      this.declaredTypes = emptySet()
      this.typeValueArgs = emptyMap<String, Int>()
      this.importAlias = emptySet()
      this.fullQualifierTypes = emptySet()
      this.usedUserTypes = emptySet()
      this.typeAliasSymbol = emptySet()
      this.parameterNames = emptySet()
    } else {
      val importDirectives = projectFiles.flatMap { it.importList?.imports ?: emptyList() }
      this.importedDeclarations = importDirectives.mapNotNull { it.toImportedClass() }.toSet()
      this.importAlias = importDirectives.mapNotNull { it.aliasName }.toSet()
      this.importedTypes = importedDeclarations.filterNot { it.isTopLevelDeclaration() }.toSet()

      val dataInClasspath: Pair<Set<FullTypeQualifier>, Set<List<String>>> =
          parseClasspathFileClassesAndPackages(classpath)
      externalTypeReferences = dataInClasspath.first + knownGeneratedTypes
      pkgsInClasspath = dataInClasspath.second

      // declaredTypes is a structural getChildrenOfType recursion (it intentionally excludes
      // local/anonymous classes in function bodies), so it stays out of the single-pass DFS below.
      this.declaredTypes = projectFiles.flatMap { it.declaredTypes() }.toSet()

      // Single-pass PSI traversal: one DFS per file collects everything the generators need,
      // replacing 9 separate whole-tree collectDescendantsOfType walks. Semantics are preserved
      // 1:1 — same node sets, same filters, and typeValueArgs keeps its per-file
      // superCall -> callExpr -> class ordering so the last-key-wins toMap is unchanged.
      val annotations = mutableListOf<KtAnnotationEntry>()
      val aliasNames = mutableSetOf<String>()
      val paramNames = mutableSetOf<String>()
      val usedTypes = mutableSetOf<KtUserType>()
      val multiSegQualifiers = mutableListOf<List<String>>()
      val interfaceUserTypes = mutableListOf<KtUserType>()
      val typeValueArgPairs = mutableListOf<Pair<String, Int>>()
      for (ktFile in projectFiles) {
        val superCallPairs = mutableListOf<Pair<String, Int>>()
        val callExprPairs = mutableListOf<Pair<String, Int>>()
        val classCtorPairs = mutableListOf<Pair<String, Int>>()
        ktFile.accept(
            object : KtTreeVisitorVoid() {
              override fun visitElement(element: PsiElement) {
                if (element is KtAnnotationEntry) {
                  annotations.add(element)
                }
                if (element is KtTypeAlias) {
                  element.name?.let { aliasNames.add(it) }
                }
                if (element is KtTypeParameter) {
                  element.name?.let { paramNames.add(it) }
                }
                if (element is KtUserType && element.calculateQualifierList().size == 1) {
                  usedTypes.add(element)
                }
                if (element is KtTypeReference) {
                  element.userTypeForQualifier()?.calculateQualifierList()?.let { qualifier ->
                    if (qualifier.size >= 2) {
                      multiSegQualifiers.add(qualifier)
                    }
                  }
                }
                if (element is KtClassOrObject) {
                  interfaceUserTypes.addAll(
                      element.getInterfaceTypes().mapNotNull { it.typeAsUserType }
                  )
                }
                if (element is KtSuperTypeCallEntry) {
                  element.typeValueArgs()?.let { superCallPairs.add(it) }
                }
                if (element is KtCallExpression) {
                  element.typeValueArgs()?.let { callExprPairs.add(it) }
                }
                if (element is KtClass) {
                  element.typeValueArgsFromSuperSecondaryConstructor()?.let {
                    classCtorPairs.add(it)
                  }
                }
                super.visitElement(element)
              }
            }
        )
        typeValueArgPairs.addAll(superCallPairs)
        typeValueArgPairs.addAll(callExprPairs)
        typeValueArgPairs.addAll(classCtorPairs)
      }

      this.annotationEntries = annotations
      this.typeAliasSymbol = aliasNames
      this.parameterNames = paramNames
      this.usedUserTypes = usedTypes
      this.interfaceTypes = interfaceUserTypes
      this.fullQualifierTypes =
          multiSegQualifiers
              .distinct()
              .map { FullTypeQualifier(it) }
              .filterNot { it.isSdkQualifier() }
              .toSet()
      // TODO: We might have multiple ctors for each type.
      // For now we're just skipping to have one (the default)
      this.typeValueArgs = typeValueArgPairs.toMap()
    }
  }

  // This will handle the super call entry for the ctor params
  private fun KtSuperTypeCallEntry.typeValueArgs(): Pair<String, Int>? {
    typeAsUserType?.referencedName?.let {
      return it to valueArguments.size
    }
    return null
  }

  // This will handle the call expression for the ctor params
  private fun KtCallExpression.typeValueArgs(): Pair<String, Int>? {
    (referenceExpression() as? KtNameReferenceExpression)?.getReferencedName()?.let {
      return it to valueArguments.size
    }
    return null
  }

  // This will handle secondary constructor calls with super delegation call
  private fun KtClass.typeValueArgsFromSuperSecondaryConstructor(): Pair<String, Int>? {
    val superTypeList = this.getChildOfType<KtSuperTypeList>()
    val superTypeCallEntry = superTypeList?.getChildOfType<KtSuperTypeCallEntry>()
    val superTypeEntries = superTypeList?.getChildrenOfType<KtSuperTypeEntry>()

    if (
        superTypeList == null ||
            superTypeEntries.isNullOrEmpty() ||
            isInterface() ||
            superTypeCallEntry != null
    ) {
      return null
    }
    val clazzBody = this.getChildOfType<KtClassBody>()
    val superSecondaryConstructor = clazzBody?.getSecondaryConstructor()?.superConstructor()
    if (superSecondaryConstructor?.isNotEmpty() == true) {
      val referenceName = superTypeEntries.first().typeAsUserType?.referencedName
      referenceName?.let {
        return it to superSecondaryConstructor.first().getDelegationCall().valueArguments.size
      }
    }
    return null
  }

  /**
   * This filters out possible class candidate while supporting delegation
   * 1. Class with default constructor at class body, assume first super type call entry is class
   * 2. Class with constructor delegation call has callee "super"
   */
  private fun KtClassOrObject.getInterfaceTypes(): List<KtSuperTypeListEntry> {
    val superTypeList = this.getChildOfType<KtSuperTypeList>()
    val superTypeCallEntry = superTypeList?.getChildOfType<KtSuperTypeCallEntry>()
    val superTypeEntries = superTypeList?.getChildrenOfType<KtSuperTypeEntry>()?.toMutableList()
    val delegatedSuperTypeEntries =
        superTypeList?.getChildrenOfType<KtDelegatedSuperTypeEntry>()?.toMutableList()
    if (
        superTypeList == null ||
            (superTypeEntries.isNullOrEmpty() && delegatedSuperTypeEntries.isNullOrEmpty())
    ) {
      return emptyList()
    }
    // Skip class body check if KtClassOrObject declaration keyword is interface
    if (isInterface()) {
      return superTypeEntries.orEmpty() + delegatedSuperTypeEntries.orEmpty()
    }
    // Assume first superTypeEntry is class if secondary constructor existed without super
    // type call entry
    if (superTypeCallEntry == null && !superTypeEntries.isNullOrEmpty()) {
      val clazzBody = this.getChildOfType<KtClassBody>()
      val secondaryConstructors = clazzBody?.getSecondaryConstructor()
      val hasDefaultConstructor = secondaryConstructors?.defaultConstructor()?.isNotEmpty() == true
      val hasSuperDelegationCallExpression =
          secondaryConstructors?.superConstructor()?.isNotEmpty() == true
      if (hasDefaultConstructor || hasSuperDelegationCallExpression) {
        superTypeEntries.removeFirstOrNull()
      }
    }
    return superTypeEntries.orEmpty() + delegatedSuperTypeEntries.orEmpty()
  }

  private fun KtClassOrObject.isInterface(): Boolean =
      getDeclarationKeyword() == KtTokens.INTERFACE_KEYWORD

  private fun KtClassBody.getSecondaryConstructor(): List<KtSecondaryConstructor> =
      getChildrenOfType<KtSecondaryConstructor>().toList()

  private fun List<KtSecondaryConstructor>.defaultConstructor() = filter { secondaryConstructor ->
    val emptyParameterList =
        secondaryConstructor.getChildOfType<KtParameterList>()?.parameters?.isEmpty() == true
    emptyParameterList && secondaryConstructor.hasImplicitDelegationCall()
  }

  private fun List<KtSecondaryConstructor>.superConstructor() = filter {
    !it.hasImplicitDelegationCall() && it.getDelegationCall().calleeExpression?.isThis == false
  }

  /**
   * The [KtUserType] directly under a [KtTypeReference], unwrapping a single nullable wrapper.
   * Mirrors the former UserTypeUtil.getUserType used by collectMultiSegmentQualifiers.
   *
   * When non-nullable: KtTypeReference - KtUserType. When nullable: KtTypeReference -
   * KtNullableType
   * - KtUserType.
   */
  private fun KtTypeReference.userTypeForQualifier(): KtUserType? {
    val nullableTypeWrapper = getChildOfType<KtNullableType>()
    return (nullableTypeWrapper ?: this).getChildOfType<KtUserType>()
  }

  fun packageName(): String? {
    return projectFiles
        .map { it -> it.packageFqName.asString() }
        .filter { it -> it.isNotEmpty() }
        .firstOrNull()
  }
}
