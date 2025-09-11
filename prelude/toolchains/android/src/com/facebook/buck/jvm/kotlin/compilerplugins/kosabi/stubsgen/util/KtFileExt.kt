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
import org.jetbrains.kotlin.psi.KtClassBody
import org.jetbrains.kotlin.psi.KtClassOrObject
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.kotlin.psi.psiUtil.getChildOfType
import org.jetbrains.kotlin.psi.psiUtil.getChildrenOfType

fun KtFile.declaredTypes(): List<FullTypeQualifier> {
  val pkg = packageFqName.pathSegments().map { it.asString() }
  return getChildrenOfType<KtClassOrObject>().flatMap { it.declaredTypes(pkg) }
}

fun KtClassOrObject.declaredTypes(prefix: List<String>): List<FullTypeQualifier> {
  // TODO: log the name == null cases (likely anonymous classes)
  val theName = name ?: return emptyList()
  val inners = getChildOfType<KtClassBody>()?.getChildrenOfType<KtClassOrObject>() ?: emptyArray()
  val qualifier = FullTypeQualifier(prefix + listOf(theName))
  return listOf(qualifier) + inners.flatMap { it.declaredTypes(qualifier.segments) }
}
