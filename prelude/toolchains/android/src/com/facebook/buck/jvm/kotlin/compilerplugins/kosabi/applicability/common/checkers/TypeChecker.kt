/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.kotlin.compilerplugins.kosabi.applicability.common.checkers

import org.jetbrains.kotlin.com.intellij.psi.PsiElement
import org.jetbrains.kotlin.descriptors.TypeParameterDescriptor
import org.jetbrains.kotlin.lexer.KtTokens
import org.jetbrains.kotlin.psi.KtBlockExpression
import org.jetbrains.kotlin.psi.KtClassOrObject
import org.jetbrains.kotlin.psi.KtImportList
import org.jetbrains.kotlin.psi.KtModifierListOwner
import org.jetbrains.kotlin.psi.KtNamedFunction
import org.jetbrains.kotlin.psi.KtNullableType
import org.jetbrains.kotlin.psi.KtPackageDirective
import org.jetbrains.kotlin.psi.KtProperty
import org.jetbrains.kotlin.psi.KtTypeReference
import org.jetbrains.kotlin.psi.KtUserType
import org.jetbrains.kotlin.psi.psiUtil.getChildOfType
import org.jetbrains.kotlin.psi.psiUtil.isPrivate
import org.jetbrains.kotlin.psi.psiUtil.visibilityModifierTypeOrDefault
import org.jetbrains.kotlin.types.KotlinType
import org.jetbrains.kotlin.types.typeUtil.constituentTypes

abstract class TypeChecker : KtCheckerBase() {
  val types: MutableList<KtTypeReference> = mutableListOf()

  abstract fun getViolationsOfType(type: KtTypeReference): List<String>

  override fun visitPackageDirective(directive: KtPackageDirective) {
    pkg = directive.qualifiedName
    super.visitPackageDirective(directive)
  }

  override val violations: List<Violation>
    get() =
        types.mapNotNull { type ->
          getViolationsOfType(type)
              .takeIf { it.isNotEmpty() }
              ?.let { violationsOfType -> Violation(type.textOffset, violationsOfType.toString()) }
        }

  fun getFilteredTypes(kotlinType: KotlinType): List<KotlinType> =
      kotlinType
          // flatten the Type. Example: List<TypeA> to {List<>, TypeA}
          .constituentTypes()
          // Skip generics. Example: class foo<H: View> {val bar: H? = null}, we need to check the
          // type View and not H
          .filterNot { it.isGeneric() }

  private fun KotlinType.isGeneric(): Boolean =
      constructor.declarationDescriptor is TypeParameterDescriptor

  private fun KtModifierListOwner.hasPrivateVisibility(): Boolean =
      visibilityModifierTypeOrDefault() == KtTokens.PRIVATE_KEYWORD

  override fun visitImportList(importList: KtImportList) {
    // We don't care about types in imports
    return
  }

  override fun visitNamedFunction(function: KtNamedFunction) {
    // We don't care about private elements
    if (function.hasPrivateVisibility()) return

    super.visitNamedFunction(function)
  }

  override fun visitProperty(property: KtProperty) {
    // We don't care about private elements
    if (property.hasPrivateVisibility()) return

    super.visitProperty(property)
  }

  override fun visitClassOrObject(classOrObject: KtClassOrObject) {
    // We don't care about private elements
    if (classOrObject.isPrivate()) return

    super.visitClassOrObject(classOrObject)
  }

  override fun visitBlockExpression(expression: KtBlockExpression) = Unit

  override fun visitTypeReference(typeReference: KtTypeReference) {
    types.add(typeReference)
    super.visitTypeReference(typeReference)
  }

  // TODO(T125743074): Move common logic like this function to Kosabi/Common
  fun KtUserType.calculateFullQualifier(): List<String> {
    val outer: MutableList<String> = mutableListOf()

    var type: KtUserType? = this
    while (type != null) {
      type.referencedName?.let { outer += it }
      type = type.qualifier
    }

    return outer.reversed()
  }

  companion object {
    lateinit var pkg: String
      private set

    // TODO(T125743074): Move common logic like this function to Kosabi/Common
    //  This function is also a part of Kosabi/Stubsgen
    fun PsiElement.getUserType(): KtUserType? {
      val nullableTypeWrapper = getChildOfType<KtNullableType>()
      return if (nullableTypeWrapper != null) {
        nullableTypeWrapper.getUserType() // This could handle nested nullable types
      } else {
        this.getChildOfType()
      }
    }
  }
}

// The type is declared in [KtFile]
fun isDeclaredLocally(jetType: String, packageFqName: String): Boolean {
  val parentPkg = getPackageName(jetType)

  // The type is declared in [pkg] scope
  return parentPkg == packageFqName
}

// The type is declared in [KtFile]
/**
 * Extract package only from a FQN.
 *
 * Example:
 * ```
 * com.example.subpkg.Foo -> com.example.subpkg
 * ```
 */
fun getPackageName(jetType: String): String {
  return jetType
      .split('.')
      .takeWhile { it.isNotEmpty() && it.first().isLowerCase() }
      .joinToString(separator = ".")
}
