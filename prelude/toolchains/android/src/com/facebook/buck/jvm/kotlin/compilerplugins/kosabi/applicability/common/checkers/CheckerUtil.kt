/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.kotlin.compilerplugins.kosabi.applicability.common.checkers

import org.jetbrains.kotlin.BlockExpressionElementType
import org.jetbrains.kotlin.KtSourceElement
import org.jetbrains.kotlin.backend.common.pop
import org.jetbrains.kotlin.backend.common.push
import org.jetbrains.kotlin.diagnostics.getAncestors
import org.jetbrains.kotlin.fir.FirElement
import org.jetbrains.kotlin.fir.analysis.checkers.FirTypeRefSource
import org.jetbrains.kotlin.fir.analysis.checkers.extractArgumentsTypeRefAndSource
import org.jetbrains.kotlin.fir.declarations.FirImport
import org.jetbrains.kotlin.fir.declarations.FirMemberDeclaration
import org.jetbrains.kotlin.fir.declarations.utils.visibility
import org.jetbrains.kotlin.fir.expressions.FirAnonymousFunctionExpression
import org.jetbrains.kotlin.fir.expressions.FirBlock
import org.jetbrains.kotlin.fir.expressions.FirPropertyAccessExpression
import org.jetbrains.kotlin.fir.toFirResolvedTypeRef
import org.jetbrains.kotlin.fir.types.FirTypeRef
import org.jetbrains.kotlin.fir.types.coneType
import org.jetbrains.kotlin.fir.types.type

fun KtSourceElement?.parentIsBlock(): Boolean {
  return this?.treeStructure?.getAncestors(lighterASTNode)?.any { n ->
    n.tokenType is BlockExpressionElementType
  } == true
}

fun FirTypeRef.constituentTypes(): List<FirTypeRef> {
  // it is important to keep order of elements to match them with KtTypeReference
  val types = linkedSetOf(this)
  coneType.typeArguments.forEach {
    it.type?.toFirResolvedTypeRef()?.constituentTypes()?.apply { types.addAll(this) }
  }
  return types.toList()
}

fun FirElement.isIrrelevantForAbi(): Boolean {
  // ignore imports list, they are explicitly imports.
  if (this is FirImport) {
    return true
  }

  // ignore blocks
  if (this is FirBlock) {
    return true
  }

  // skip function expressions
  if (this is FirAnonymousFunctionExpression) {
    return true
  }

  // skip property initializers
  if (this is FirPropertyAccessExpression) {
    // Since we mangle initializers in codegen, there's no need to check them here
    return true
  }

  // we ignore typing in anon functions including lambdas as last params
  // example: combine(a, b, c) { list -> doSomething() }
  if (this is FirAnonymousFunctionExpression && this.isTrailingLambda) {
    return true
  }

  // we ignore private members because they do not affect abi
  if ((this as? FirMemberDeclaration)?.visibility?.isPublicAPI == false) {
    return true
  }

  return false
}

inline fun FirTypeRef.forEachFirTypeRefSource(
    action: (FirTypeRefSource) -> Unit,
) {
  val stack = mutableListOf(FirTypeRefSource(this, this.source))

  while (stack.isNotEmpty()) {
    val firTypeRefSource = stack.pop()
    action(firTypeRefSource)

    extractArgumentsTypeRefAndSource(firTypeRefSource.typeRef)?.forEach(stack::push)
  }
}
