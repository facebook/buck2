/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.kotlin.compilerplugins.kosabi.stubsgen.util

import org.jetbrains.kotlin.com.intellij.psi.PsiElement
import org.jetbrains.kotlin.lexer.KtTokens
import org.jetbrains.kotlin.psi.KtConstantExpression
import org.jetbrains.kotlin.psi.KtPrefixExpression
import org.jetbrains.kotlin.psi.KtStringTemplateExpression
import org.jetbrains.kotlin.psi.psiUtil.getChildOfType
import org.jetbrains.kotlin.psi.psiUtil.isNull

// Primitive Data Types:
// - Int, Short, Long, Float...
// - Char, String
// - Boolean
fun PsiElement.isPrimitiveDataType(): Boolean =
    isConstantLiteral() || isStringLiteral() || isNegativeConstantLiteral()

fun PsiElement.isConstantLiteral(): Boolean = this is KtConstantExpression

fun PsiElement.isStringLiteral(): Boolean = this is KtStringTemplateExpression

fun PsiElement.isNegativeConstantLiteral(): Boolean =
    if (this is KtPrefixExpression) {
      operationToken == KtTokens.MINUS && getChildOfType<KtConstantExpression>()?.isNull() == false
    } else {
      false
    }
