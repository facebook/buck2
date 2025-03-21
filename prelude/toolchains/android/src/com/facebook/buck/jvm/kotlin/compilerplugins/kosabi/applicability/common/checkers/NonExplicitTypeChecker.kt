/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.kotlin.compilerplugins.kosabi.applicability.common.checkers

import com.facebook.kotlin.compilercompat.getContainingClassCompat as getContainingClass
import com.facebook.kotlin.compilerplugins.kosabi.applicability.common.CheckerFix
import com.facebook.kotlin.compilerplugins.kosabi.applicability.common.CheckerFixType
import com.facebook.kotlin.compilerplugins.kosabi.applicability.common.checkers.errors.FirApplicabilityErrors.IMPLICIT_FUNCTION_RETURN_TYPE
import com.facebook.kotlin.compilerplugins.kosabi.applicability.common.checkers.errors.FirApplicabilityErrors.IMPLICIT_PROPERTY_TYPE
import org.jetbrains.kotlin.KtFakeSourceElementKind
import org.jetbrains.kotlin.KtRealSourceElementKind
import org.jetbrains.kotlin.KtSourceElementKind
import org.jetbrains.kotlin.com.intellij.psi.PsiElement
import org.jetbrains.kotlin.descriptors.ConstructorDescriptor
import org.jetbrains.kotlin.descriptors.EffectiveVisibility
import org.jetbrains.kotlin.diagnostics.DiagnosticReporter
import org.jetbrains.kotlin.diagnostics.reportOn
import org.jetbrains.kotlin.fir.FirSession
import org.jetbrains.kotlin.fir.analysis.checkers.MppCheckerKind
import org.jetbrains.kotlin.fir.analysis.checkers.context.CheckerContext
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.DeclarationCheckers
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.FirFunctionChecker
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.FirPropertyChecker
import org.jetbrains.kotlin.fir.declarations.FirAnonymousFunction
import org.jetbrains.kotlin.fir.declarations.FirCallableDeclaration
import org.jetbrains.kotlin.fir.declarations.FirFunction
import org.jetbrains.kotlin.fir.declarations.FirProperty
import org.jetbrains.kotlin.fir.declarations.impl.FirDefaultPropertyAccessor
import org.jetbrains.kotlin.fir.declarations.impl.FirPrimaryConstructor
import org.jetbrains.kotlin.fir.declarations.utils.effectiveVisibility
import org.jetbrains.kotlin.fir.declarations.utils.visibility
import org.jetbrains.kotlin.fir.expressions.FirExpression
import org.jetbrains.kotlin.fir.expressions.FirFunctionCall
import org.jetbrains.kotlin.fir.expressions.FirLiteralExpression
import org.jetbrains.kotlin.fir.expressions.FirOperation
import org.jetbrains.kotlin.fir.expressions.FirReturnExpression
import org.jetbrains.kotlin.fir.expressions.FirStringConcatenationCall
import org.jetbrains.kotlin.fir.expressions.FirTypeOperatorCall
import org.jetbrains.kotlin.fir.expressions.arguments
import org.jetbrains.kotlin.fir.expressions.impl.FirFunctionCallImpl
import org.jetbrains.kotlin.fir.expressions.impl.FirSingleExpressionBlock
import org.jetbrains.kotlin.fir.packageFqName
import org.jetbrains.kotlin.fir.references.resolved
import org.jetbrains.kotlin.fir.references.symbol
import org.jetbrains.kotlin.fir.symbols.impl.FirConstructorSymbol
import org.jetbrains.kotlin.fir.symbols.impl.FirNamedFunctionSymbol
import org.jetbrains.kotlin.fir.types.coneType
import org.jetbrains.kotlin.fir.types.impl.FirImplicitUnitTypeRef
import org.jetbrains.kotlin.lexer.KtTokens
import org.jetbrains.kotlin.psi.KtBinaryExpressionWithTypeRHS
import org.jetbrains.kotlin.psi.KtBlockExpression
import org.jetbrains.kotlin.psi.KtCallExpression
import org.jetbrains.kotlin.psi.KtClass
import org.jetbrains.kotlin.psi.KtConstantExpression
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.kotlin.psi.KtNamedFunction
import org.jetbrains.kotlin.psi.KtPrefixExpression
import org.jetbrains.kotlin.psi.KtProperty
import org.jetbrains.kotlin.psi.KtStringTemplateExpression
import org.jetbrains.kotlin.psi.psiUtil.containingClass
import org.jetbrains.kotlin.psi.psiUtil.getChildOfType
import org.jetbrains.kotlin.psi.psiUtil.isNull
import org.jetbrains.kotlin.psi.psiUtil.isPrivate
import org.jetbrains.kotlin.psi.psiUtil.visibilityModifierTypeOrDefault
import org.jetbrains.kotlin.psi.stubs.elements.KtPropertyAccessorElementType
import org.jetbrains.kotlin.resolve.BindingContext
import org.jetbrains.kotlin.resolve.calls.util.getResolvedCall
import org.jetbrains.kotlin.text

/* Check for non private fields and functions with explicit types*/
class NonExplicitTypeChecker(private val bindingContext: BindingContext) : KtCheckerBase() {
  override val violations: MutableList<Violation> = mutableListOf()

  override fun visitNamedFunction(function: KtNamedFunction) {
    if (function.isNonExplicitType()) {
      violations.add(Violation(function.textOffset))
    }
  }

  override fun visitProperty(property: KtProperty) {
    if (property.isNonExplicitType(bindingContext)) {
      violations.add(Violation(property.textOffset))
    }
  }

  override fun visitBlockExpression(expression: KtBlockExpression) = Unit

  override fun visitClass(klass: KtClass) {
    if (klass.isPrivate()) {
      return
    }
    super.visitClass(klass)
  }

  companion object : FeatureChecker {
    override val name: String = "Non explicit types"
    override val description: String =
        "This module miss explicit non-private types on several functions/properties."
    override val fix: CheckerFix =
        CheckerFix(
            CheckerFixType.CODEMOD,
            """
        Refer to https://fburl.com/wiki/s77y5ctu for instructions on running codemod to fix implicit non-private types.

        We found implicit non-private types in the following files:
      """
                .trimIndent())

    override val declarationCheckers: DeclarationCheckers
      get() =
          object : DeclarationCheckers() {
            override val propertyCheckers: Set<FirPropertyChecker>
              get() = setOf(NonExplicitTypePropertyCheckerK2())

            override val functionCheckers: Set<FirFunctionChecker>
              get() = setOf(NonExplicitTypeFunctionCheckerK2())
          }

    override fun ktFileChecker(file: KtFile, bindingContext: BindingContext): KtCheckerBase =
        NonExplicitTypeChecker(bindingContext)
  }
}

private const val UNIT_BODY_EXPRESSION = "Unit"

private val AS_OPERATIONS = listOf(FirOperation.SAFE_AS, FirOperation.AS)

// Should detect: fun foo() = Unit
private fun KtNamedFunction.isExactlyEqUnit(): Boolean =
    bodyExpression?.textMatches(UNIT_BODY_EXPRESSION) ?: false

// interface I {
//   fun foo()
// }
private fun KtNamedFunction.isInterfaceFunctionReturningUnit(): Boolean =
    (containingClass()?.isInterface() ?: false) &&
        !hasDeclaredReturnType() &&
        bodyExpression == null

// external fun foo()
private fun KtNamedFunction.isExternalFunctionReturningUnit(): Boolean =
    hasModifier(KtTokens.EXTERNAL_KEYWORD) && !hasDeclaredReturnType()

private fun KtNamedFunction.isAbstractFunctionReturningUnit(): Boolean =
    hasModifier(KtTokens.ABSTRACT_KEYWORD) && !hasDeclaredReturnType()

private fun <T> T.isLiteralType(): Boolean where T : PsiElement =
    isStringLiteralType() || isConstantLiteralType() || isNegativeConstantLiteralType()

private fun KtNamedFunction.isNonExplicitType(): Boolean {
  return visibilityModifierTypeOrDefault() != KtTokens.PRIVATE_KEYWORD &&
      // An indicator of `fun foo(): [Maybe type here] = ...`
      bodyBlockExpression == null &&
      // An indicator of `fun foo() [No type] = ...`
      !hasDeclaredReturnType() &&
      // We're not in `fun foo() = Unit` case
      !isExactlyEqUnit() &&
      // We're not in `interface I { fun foo() }`
      !isInterfaceFunctionReturningUnit() &&
      // We're not in `external fun foo()`
      !isExternalFunctionReturningUnit() &&
      !isAbstractFunctionReturningUnit() &&
      !isLiteralType() &&
      !hasAsBinaryWithType()
}

private fun FirFunction.returnExpression(): FirReturnExpression? =
    (body as? FirSingleExpressionBlock)?.statement as? FirReturnExpression

private fun FirExpression.isAsOperation(): Boolean =
    (this as? FirTypeOperatorCall)?.operation in AS_OPERATIONS

private fun FirExpression.isConstructorCall(): Boolean =
    (this as? FirFunctionCall)?.calleeReference?.resolved?.resolvedSymbol is FirConstructorSymbol

private fun FirExpression.isConstructorCallWithoutNonLiteralParams(): Boolean {
  return (this as? FirFunctionCall)?.calleeReference?.resolved?.resolvedSymbol is
      FirConstructorSymbol &&
      (this as? FirFunctionCall)?.arguments?.all { exp ->
        exp is FirLiteralExpression || exp is FirStringConcatenationCall
      } ?: true
}

private fun FirFunction.hasAsBinary(): Boolean =
    returnExpression()?.result?.isAsOperation() ?: false

private fun FirFunction.isNonExplicitType(): Boolean {
  // we only check public API
  return !effectiveVisibility.privateApi &&
      // local declarations, for example inside rhs objects:
      // val obj = object: Something() {...}
      effectiveVisibility != EffectiveVisibility.Local &&
      // we do not check default property accessors
      this !is FirDefaultPropertyAccessor &&
      // Primary constructor return type is always SELF type
      this !is FirPrimaryConstructor &&
      // functions without explicitly specified return type and single body expression, e.g
      // fun something() = ...
      // literal types, functions with declared return types, interface functions returning unit,
      // abstract functions returning unit, functions with bodies
      // do not qualify as ImplicitTypeRef
      returnTypeRef !is FirImplicitUnitTypeRef &&
      // allows something() = Unit declarations
      !isExactlyEqUnit() &&
      returnTypeRef.source?.kind.isDisallowedImplicit() &&
      !isLiteralType() &&
      !hasAsBinary()
}

private fun KtSourceElementKind?.isDisallowedImplicit(): Boolean =
    when (this) {
      is KtRealSourceElementKind,
      is KtFakeSourceElementKind.ClassSelfTypeRef,
      is KtFakeSourceElementKind.DataClassGeneratedMembers,
      is KtFakeSourceElementKind.EnumGeneratedDeclaration,
      is KtFakeSourceElementKind.PropertyFromParameter -> false
      else -> true
    }

private fun FirExpression.isLiteralType(): Boolean =
    this is FirLiteralExpression ||
        this is FirStringConcatenationCall ||
        this.isNegativeConstantLiteralType()

private fun FirFunction.isLiteralType(): Boolean {
  val returnExpression = this.returnExpression() ?: return false
  return returnExpression.result.isLiteralType()
}

private fun FirProperty.isLiteralType(): Boolean = initializer?.isLiteralType() ?: false

private fun FirProperty.hasAsBinary(): Boolean = initializer?.isAsOperation() ?: false

private fun FirProperty.isConstructorCall(): Boolean = initializer?.isConstructorCall() ?: false

private fun FirProperty.isConstructorCallWithoutNonLiteralParams(): Boolean =
    initializer?.isConstructorCallWithoutNonLiteralParams() ?: false

private fun KtProperty.isNonExplicitType(bindingContext: BindingContext): Boolean {
  return visibilityModifierTypeOrDefault() != KtTokens.PRIVATE_KEYWORD &&
      typeReference == null &&
      (isMember || isTopLevel) &&
      !isLiteralType() &&
      !hasAsBinaryWithType() &&
      !hasConstructorCallWithoutNonLiteralParams(bindingContext)
}

private fun KtProperty.hasConstructorCallWithoutNonLiteralParams(
    bindingContext: BindingContext
): Boolean {
  val resolvedCall =
      getChildOfType<KtCallExpression>()?.calleeExpression?.getResolvedCall(bindingContext)
  return resolvedCall?.candidateDescriptor is ConstructorDescriptor &&
      resolvedCall?.valueArguments?.values?.all {
        it.arguments.all { arg -> arg.getArgumentExpression()?.isLiteralType() ?: true }
      } ?: true
}

private fun FirProperty.isNonExplicitType(): Boolean {
  return !effectiveVisibility.privateApi &&
      // local declarations, for example inside rhs objects:
      // val obj = object: Something() {...}
      effectiveVisibility != EffectiveVisibility.Local &&
      // property type is not specified explicitly
      returnTypeRef.source?.kind.isDisallowedImplicit() &&
      !hasAsBinary() &&
      !isLiteralType() &&
      !isConstructorCallWithoutNonLiteralParams()
}

// String literal type
// val stringName = "something"
// fun getString() = "string"
private fun <T> T.isStringLiteralType(): Boolean where T : PsiElement =
    this is KtStringTemplateExpression ||
        getChildOfType<KtStringTemplateExpression>()?.isNull() == false

// Numeric/Boolean/Character literal types
// val number = 10
// val something = 9.12321
// val isSomethingWrong = false
// val char = 'A'
private fun <T> T.isConstantLiteralType(): Boolean where T : PsiElement =
    this is KtConstantExpression || getChildOfType<KtConstantExpression>() != null

// Signed Numeric number with negative value
// val num = -1
// fun getSomeValue() = -100L
private fun <T> T.isNegativeConstantLiteralType(): Boolean where T : PsiElement =
    getChildOfType<KtPrefixExpression>()?.let {
      it.operationToken == KtTokens.MINUS &&
          it.getChildOfType<KtConstantExpression>()?.isNull() == false
    } ?: run { false }

// Signed Numeric number with negative value
// val num = -1
// in FIR these expressions are represented as 1.unaryMinus()
private fun FirExpression.isNegativeConstantLiteralType(): Boolean =
    ((this as? FirFunctionCallImpl)?.calleeReference?.symbol as? FirNamedFunctionSymbol)
        ?.packageFqName()
        ?.asString() == "kotlin" &&
        this.calleeReference.name.identifier == "unaryMinus" &&
        this.explicitReceiver is FirLiteralExpression

// val something = thing() as List<Date>
// val some = 5 as Float
// val type = A as B as C
// fun aFunction() = anotherFunction() as? Something
private fun <T> T.hasAsBinaryWithType(): Boolean where T : PsiElement =
    getChildOfType<KtBinaryExpressionWithTypeRHS>()
        ?.operationReference
        ?.getReferencedNameElementType() in listOf(KtTokens.AS_KEYWORD, KtTokens.AS_SAFE)

private fun FirCallableDeclaration.isParentPrivate(session: FirSession): Boolean {
  return getContainingClass(session)?.visibility?.isPublicAPI == false
}

private fun FirFunction.isExactlyEqUnit(): Boolean = this.body?.source?.text == UNIT_BODY_EXPRESSION

private class NonExplicitTypeFunctionCheckerK2 : FirFunctionChecker(MppCheckerKind.Common) {
  override fun check(
      declaration: FirFunction,
      context: CheckerContext,
      reporter: DiagnosticReporter
  ) {
    if (!declaration.isParentPrivate(context.session) &&
        !declaration.source.parentIsBlock() &&
        declaration !is FirAnonymousFunction &&
        // properties are checked separately in NonExplicitTypePropertyCheckerK2
        declaration.source?.elementType !is KtPropertyAccessorElementType &&
        declaration.isNonExplicitType()) {
      reporter.reportOn(
          declaration.source,
          IMPLICIT_FUNCTION_RETURN_TYPE,
          declaration.returnTypeRef.coneType,
          declaration.symbol.name.asString(),
          context)
    }
  }
}

private class NonExplicitTypePropertyCheckerK2 : FirPropertyChecker(MppCheckerKind.Common) {
  override fun check(
      declaration: FirProperty,
      context: CheckerContext,
      reporter: DiagnosticReporter
  ) {
    if (!declaration.isParentPrivate(context.session) &&
        !declaration.isLocal &&
        declaration.isNonExplicitType()) {
      reporter.reportOn(
          declaration.source,
          IMPLICIT_PROPERTY_TYPE,
          declaration.returnTypeRef.coneType,
          declaration.symbol.name.asString(),
          context)
    }
  }
}
