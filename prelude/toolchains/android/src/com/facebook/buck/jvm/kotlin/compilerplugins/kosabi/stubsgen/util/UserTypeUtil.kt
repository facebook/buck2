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

import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.generators.GenerationContext
import org.jetbrains.kotlin.psi.KtNullableType
import org.jetbrains.kotlin.psi.KtTypeReference
import org.jetbrains.kotlin.psi.KtUserType
import org.jetbrains.kotlin.psi.psiUtil.collectDescendantsOfType
import org.jetbrains.kotlin.psi.psiUtil.getChildOfType

object UserTypeUtil {
  /**
   * Multi-segment qualifier:
   * - com.company.project.Type.Inner
   * - com.Type
   * - Type.Inner
   *
   * Single segment qualifiers:
   * - Type
   */
  fun collectMultiSegmentQualifiers(context: GenerationContext): List<List<String>> {
    return collectUserTypeQualifiers(context).filter { it.size >= 2 }
  }

  private fun collectUserTypeQualifiers(context: GenerationContext): List<List<String>> {
    return context.projectFiles
        .flatMap { it.collectDescendantsOfType<KtTypeReference>() }
        .mapNotNull { it.getUserType() }
        .map { it.calculateQualifierList() }
        .distinct()
  }

  /**
   * When type is non-nullable: KtTypeReference \- KtUserType
   *
   * When type is nullable: KtTypeReference \- KtNullableType \- KtUserType
   */
  private fun KtTypeReference.getUserType(): KtUserType? {
    val nullableTypeWrapper = getChildOfType<KtNullableType>()
    return (nullableTypeWrapper ?: this).getChildOfType<KtUserType>()
  }
}
