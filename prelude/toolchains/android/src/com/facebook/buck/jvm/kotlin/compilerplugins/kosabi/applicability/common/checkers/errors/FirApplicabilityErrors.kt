/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.kotlin.compilerplugins.kosabi.applicability.common.checkers.errors

import org.jetbrains.kotlin.com.intellij.psi.PsiElement
import org.jetbrains.kotlin.diagnostics.KtDiagnosticFactory1
import org.jetbrains.kotlin.diagnostics.KtDiagnosticFactory2
import org.jetbrains.kotlin.diagnostics.SourceElementPositioningStrategies
import org.jetbrains.kotlin.diagnostics.error1
import org.jetbrains.kotlin.diagnostics.error2
import org.jetbrains.kotlin.diagnostics.rendering.RootDiagnosticRendererFactory
import org.jetbrains.kotlin.fir.symbols.FirBasedSymbol
import org.jetbrains.kotlin.fir.symbols.impl.FirClassSymbol
import org.jetbrains.kotlin.fir.symbols.impl.FirPropertySymbol
import org.jetbrains.kotlin.fir.types.ConeKotlinType

// TODO(T189754040): Move to k2 module
internal object FirApplicabilityErrors {

  val IMPLICIT_IMPORT: KtDiagnosticFactory1<ConeKotlinType> by error1<PsiElement, ConeKotlinType>()
  val BLOCKED_IMPORT: KtDiagnosticFactory1<String> by error1<PsiElement, String>()
  val IMPLICIT_CONSTANT: KtDiagnosticFactory1<FirPropertySymbol> by
      error1<PsiElement, FirPropertySymbol>()
  val STAR_IMPORT: KtDiagnosticFactory1<String> by error1<PsiElement, String>()
  val INLINE_KEYWORD: KtDiagnosticFactory1<FirBasedSymbol<*>> by
      error1<PsiElement, FirBasedSymbol<*>>(SourceElementPositioningStrategies.INLINE_FUN_MODIFIER)
  val TYPE_ALIAS: KtDiagnosticFactory1<ConeKotlinType> by error1<PsiElement, ConeKotlinType>()
  val SUPER_DELEGATION: KtDiagnosticFactory2<String, ConeKotlinType> by
      error2<PsiElement, String, ConeKotlinType>()
  val IMPLICIT_FUNCTION_RETURN_TYPE: KtDiagnosticFactory2<ConeKotlinType, String> by
      error2<PsiElement, ConeKotlinType, String>(
          SourceElementPositioningStrategies.DECLARATION_NAME)
  val IMPLICIT_PROPERTY_TYPE: KtDiagnosticFactory2<ConeKotlinType, String> by
      error2<PsiElement, ConeKotlinType, String>(
          SourceElementPositioningStrategies.DECLARATION_NAME)
  val INHERITANCE_ORDER_SUPERCLASS_FIRST: KtDiagnosticFactory1<ConeKotlinType> by
      error1<PsiElement, ConeKotlinType>()
  val INHERITANCE_ORDER_NO_PRIMARY_CONSTRUCTOR: KtDiagnosticFactory1<FirClassSymbol<*>> by
      error1<PsiElement, FirClassSymbol<*>>(
          SourceElementPositioningStrategies.DECLARATION_NAME_ONLY)

  init {
    RootDiagnosticRendererFactory.registerFactory(DefaultErrorMessagesApplicability)
  }
}
