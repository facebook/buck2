/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.kotlin.compilerplugins.kosabi.applicability.common.checkers.errors

import org.jetbrains.kotlin.diagnostics.KtDiagnosticFactoryToRendererMap
import org.jetbrains.kotlin.diagnostics.KtDiagnosticRenderers
import org.jetbrains.kotlin.diagnostics.rendering.BaseDiagnosticRendererFactory
import org.jetbrains.kotlin.fir.analysis.diagnostics.FirDiagnosticRenderers

// TODO(T189754040): Move to k2 module
internal object DefaultErrorMessagesApplicability : BaseDiagnosticRendererFactory() {

  override val MAP: KtDiagnosticFactoryToRendererMap
    get() =
        KtDiagnosticFactoryToRendererMap("Applicability").apply {
          put(
              FirApplicabilityErrors.IMPLICIT_IMPORT,
              "Kosabi error: Implicitly imported types: ''{0}''. " +
                  "Please add explicit import for the type. " +
                  "For more details, refer to Wiki: https://fburl.com/wiki/v12yvkzn",
              FirDiagnosticRenderers.RENDER_TYPE,
          )
          put(
              FirApplicabilityErrors.BLOCKED_IMPORT,
              "Kosabi error: Blocked imports: ''{0}''. " +
                  "Please remove the import or use a different abi generation mode. " +
                  "For more details, refer to Wiki: https://fburl.com/wiki/z31rhecw",
              KtDiagnosticRenderers.TO_STRING)
          put(
              // TODO(T190712362): Add missing wiki
              FirApplicabilityErrors.IMPLICIT_CONSTANT,
              "Kosabi error: Implicit value assigned to const: ''{0}''. " +
                  "Please add an explicit primitive value for the constant, " +
                  "or make it private, " +
                  "or remove the const modifier. " +
                  "If in required_for_source_only_abi, " +
                  "use @SuppressLint(\"KosabiApplicabilityImplicitConstantValue\"). " +
                  "For more details, refer to Wiki: https://fburl.com/kosabi",
              FirDiagnosticRenderers.VARIABLE_NAME)
          put(
              FirApplicabilityErrors.STAR_IMPORT,
              "Kosabi error: Star imports: ''{0}''. " +
                  "Please remove the star import. " +
                  "For more details, refer to Wiki: https://fburl.com/wiki/r8jxo7p0",
              KtDiagnosticRenderers.TO_STRING)
          put(
              FirApplicabilityErrors.INLINE_KEYWORD,
              "Kosabi error: Inline keyword: ''{0}''. " +
                  "Please remove the inline modifier or make it private. " +
                  "For more details, refer to Wiki: https://fburl.com/wiki/cn1zsoe3",
              FirDiagnosticRenderers.SYMBOL)
          put(
              FirApplicabilityErrors.TYPE_ALIAS,
              "Kosabi error: Typealias declarations: ''{0}''. " +
                  "Please remove the type alias or create a new one in your file. " +
                  "For more details, refer to Wiki: https://fburl.com/wiki/ioughbsf",
              FirDiagnosticRenderers.RENDER_TYPE)
          put(
              FirApplicabilityErrors.SUPER_DELEGATION,
              "Kosabi error: Super Delegation: ''{0}''. " +
                  "Please place delegated interface ''{1}'' within the same package or " +
                  "create an implementation class for it utilising delegation and extend the class instead. " +
                  "For more details, refer to Wiki: https://fburl.com/wiki/kbp5ttsn",
              KtDiagnosticRenderers.TO_STRING,
              FirDiagnosticRenderers.RENDER_TYPE)
          put(
              FirApplicabilityErrors.IMPLICIT_FUNCTION_RETURN_TYPE,
              "Kosabi error: Non explicit types: ''{0}''. " +
                  "Please specify return type of function ''{1}'' explicitly or " +
                  "run CODEMOD to fix implicit non-private types. " +
                  "For more details, refer to Wiki: https://fburl.com/wiki/s77y5ctu",
              FirDiagnosticRenderers.RENDER_TYPE,
              KtDiagnosticRenderers.TO_STRING,
          )
          put(
              FirApplicabilityErrors.IMPLICIT_PROPERTY_TYPE,
              "Kosabi error: Non explicit types: ''{0}''. " +
                  "Please specify type of property ''{1}'' explicitly or " +
                  "run CODEMOD to fix implicit non-private types. " +
                  "For more details, refer to Wiki: https://fburl.com/wiki/s77y5ctu",
              FirDiagnosticRenderers.RENDER_TYPE,
              KtDiagnosticRenderers.TO_STRING,
          )
          put(
              FirApplicabilityErrors.INHERITANCE_ORDER_SUPERCLASS_FIRST,
              "Kosabi error: Inheritance: Invalid supertypes order. " +
                  "Please move the superclass ''{0}'' to be the first in the list of supertypes. " +
                  "For more details, refer to Wiki: https://fburl.com/wiki/adqptnf2",
              FirDiagnosticRenderers.RENDER_TYPE,
          )
          put(
              FirApplicabilityErrors.INHERITANCE_ORDER_NO_PRIMARY_CONSTRUCTOR,
              "Kosabi error: Inheritance: Missing primary constructor. " +
                  "Please add a primary constructor for class ''{0}''. " +
                  "For more details, refer to Wiki: https://fburl.com/wiki/adqptnf2",
              FirDiagnosticRenderers.DECLARATION_NAME,
          )
        }
}
