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
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.util.UserTypeUtil
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.util.calculateQualifierList
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.util.declaredTypes
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.util.toImportedClass
import java.io.File
import org.jetbrains.kotlin.lexer.KtTokens
import org.jetbrains.kotlin.psi.KtAnnotationEntry
import org.jetbrains.kotlin.psi.KtCallExpression
import org.jetbrains.kotlin.psi.KtClass
import org.jetbrains.kotlin.psi.KtClassBody
import org.jetbrains.kotlin.psi.KtClassOrObject
import org.jetbrains.kotlin.psi.KtDelegatedSuperTypeEntry
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.kotlin.psi.KtNameReferenceExpression
import org.jetbrains.kotlin.psi.KtParameterList
import org.jetbrains.kotlin.psi.KtSecondaryConstructor
import org.jetbrains.kotlin.psi.KtSuperTypeCallEntry
import org.jetbrains.kotlin.psi.KtSuperTypeEntry
import org.jetbrains.kotlin.psi.KtSuperTypeList
import org.jetbrains.kotlin.psi.KtSuperTypeListEntry
import org.jetbrains.kotlin.psi.KtTypeAlias
import org.jetbrains.kotlin.psi.KtTypeParameter
import org.jetbrains.kotlin.psi.KtUserType
import org.jetbrains.kotlin.psi.psiUtil.collectDescendantsOfType
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
      this.fullQualifierTypes =
          UserTypeUtil.collectMultiSegmentQualifiers(this)
              .map { FullTypeQualifier(it) }
              .filterNot { it.isSdkQualifier() }
              .toSet()
      val dataInClasspath: Pair<Set<FullTypeQualifier>, Set<List<String>>> =
          parseClasspathFileClassesAndPackages(classpath)
      externalTypeReferences = dataInClasspath.first + knownGeneratedTypes
      pkgsInClasspath = dataInClasspath.second

      this.interfaceTypes =
          projectFiles
              .flatMap { it.getInterfaceTypes() }
              // TODO: Could it be null?? at any point? Why?
              // TODO: log these cases
              .mapNotNull { it.typeAsUserType }

      this.annotationEntries = projectFiles.flatMap { it.collectDescendantsOfType() }

      this.typeAliasSymbol =
          projectFiles
              .flatMap { it.collectDescendantsOfType<KtTypeAlias>() }
              .mapNotNull { it.name }
              .toSet()

      this.declaredTypes = projectFiles.flatMap { it.declaredTypes() }.toSet()
      this.usedUserTypes =
          projectFiles
              .flatMap {
                it.collectDescendantsOfType<KtUserType> { userType ->
                  userType.calculateQualifierList().size == 1
                }
              }
              .toSet()

      this.parameterNames =
          projectFiles
              .flatMap {
                it.collectDescendantsOfType<KtTypeParameter>().mapNotNull { it -> it.name }
              }
              .toSet()

      this.typeValueArgs =
          projectFiles
              .flatMap { ktFile ->
                val superTypeCallEntryArgsPairs =
                    ktFile.collectDescendantsOfType<KtSuperTypeCallEntry>().mapNotNull { entry ->
                      entry.typeValueArgs()
                    }
                val callExpressionArgsPairs =
                    ktFile.collectDescendantsOfType<KtCallExpression>().mapNotNull { expr ->
                      expr.typeValueArgs()
                    }
                val classSuperSecondaryConstructorArgsPairs =
                    ktFile.collectDescendantsOfType<KtClass>().mapNotNull { clazz ->
                      clazz.typeValueArgsFromSuperSecondaryConstructor()
                    }
                superTypeCallEntryArgsPairs +
                    callExpressionArgsPairs +
                    classSuperSecondaryConstructorArgsPairs
              }
              // TODO: We might have multiple ctors for each type.
              // For now we're just skipping to have one (the default)
              .toMap()
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

  private fun KtFile.getInterfaceTypes(): List<KtSuperTypeListEntry> =
      collectDescendantsOfType<KtClassOrObject>().flatMap { it.getInterfaceTypes() }

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

  fun packageName(): String? {
    return projectFiles
        .map { it -> it.packageFqName.asString() }
        .filter { it -> it.isNotEmpty() }
        .firstOrNull()
  }
}
