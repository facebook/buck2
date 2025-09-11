/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.kotlin.compilerplugins.kosabi.stubsgen.util

import com.facebook.kotlin.compilerplugins.kosabi.common.FullTypeQualifier
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.generators.GenerationContext
import org.jetbrains.kotlin.com.intellij.psi.PsiElement
import org.jetbrains.kotlin.psi.KtClassLiteralExpression
import org.jetbrains.kotlin.psi.KtDotQualifiedExpression
import org.jetbrains.kotlin.psi.KtExpression
import org.jetbrains.kotlin.psi.KtNameReferenceExpression
import org.jetbrains.kotlin.psi.psiUtil.collectDescendantsOfType

/**
 * Class reference is a Kotlin reflection, it is presented in KtClassLiteralExpression
 * KtClassLiteralExpression could be:
 * - SomeClass::class
 * - SomeClass.SomeName::class
 * - java.io.IOException::class
 */
object ClassReferenceUtil {

  fun collectMultiSegmentQualifiers(context: GenerationContext): List<List<String>> {
    return context.projectFiles
        .flatMap { it.collectDescendantsOfType<KtClassLiteralExpression>() }
        .mapNotNull { it.lhs }
        .map { it.calculateQualifiers() }
        .filter { it.isNotEmpty() }
        .filter { FullTypeQualifier(it)?.isTopLevelDeclaration() == false }
        .distinct()
        .toList()
  }
}

/**
 * This function decodes the KtDotQualifiedExpression and visit its child accordingly to get the
 * qualifiers.
 *
 * KtDotQualifiedExpression: Outer.A.SomeException
 * - selector: SomeException
 * - receiver: Outer.A (KtDotQualifiedExpression)
 * - reversedQualifiers: ["SomeException"]
 *
 * KtDotQualifiedExpression: Outer.A
 * - selector: A
 * - receiver: Outer
 * - ["SomeException", "A", "Outer"]
 *
 * result.reversed(): ["Outer", "A", "SomeException"]
 */
private fun PsiElement.calculateQualifiers(): List<String> {
  val reversedQualifiers: MutableList<String> = mutableListOf()
  if (this is KtDotQualifiedExpression) {
    var dotQualifiedVisitor: KtDotQualifiedExpression? = this
    while (dotQualifiedVisitor != null) {
      dotQualifiedVisitor.selectorExpression?.getReferencedName()?.let {
        reversedQualifiers.add(it)
      }
      val receiver = dotQualifiedVisitor.receiverExpression
      dotQualifiedVisitor =
          if (receiver is KtDotQualifiedExpression) {
            receiver
          } else {
            receiver.getReferencedName()?.let { reversedQualifiers.add(it) }
            null
          }
    }
  }
  return reversedQualifiers.reversed()
}

private fun KtExpression.getReferencedName(): String? =
    if (this is KtNameReferenceExpression) {
      getReferencedName()
    } else {
      null
    }
