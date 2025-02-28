/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.kotlin.compilerplugins.kosabi.applicability.k2

import com.facebook.kotlin.compilerplugins.kosabi.applicability.common.checkers.FeatureChecker
import org.jetbrains.kotlin.fir.FirSession
import org.jetbrains.kotlin.fir.analysis.cfa.AbstractFirPropertyInitializationChecker
import org.jetbrains.kotlin.fir.analysis.checkers.cfa.FirControlFlowChecker
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.DeclarationCheckers
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.FirAnonymousFunctionChecker
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.FirAnonymousInitializerChecker
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.FirAnonymousObjectChecker
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.FirBackingFieldChecker
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.FirBasicDeclarationChecker
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.FirCallableDeclarationChecker
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.FirClassChecker
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.FirClassLikeChecker
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.FirConstructorChecker
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.FirEnumEntryChecker
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.FirFileChecker
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.FirFunctionChecker
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.FirPropertyAccessorChecker
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.FirPropertyChecker
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.FirRegularClassChecker
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.FirSimpleFunctionChecker
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.FirTypeAliasChecker
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.FirTypeParameterChecker
import org.jetbrains.kotlin.fir.analysis.checkers.declaration.FirValueParameterChecker
import org.jetbrains.kotlin.fir.analysis.extensions.FirAdditionalCheckersExtension

class ApplicabilityPluginExtension(
    session: FirSession,
    private val checkers: List<FeatureChecker>
) : FirAdditionalCheckersExtension(session) {
  override val declarationCheckers: DeclarationCheckers
    get() =
        object : DeclarationCheckers() {
          override val anonymousObjectCheckers: Set<FirAnonymousObjectChecker>
            get() = checkers.flatMap { it.declarationCheckers.anonymousObjectCheckers }.toSet()

          override val anonymousInitializerCheckers: Set<FirAnonymousInitializerChecker>
            get() = checkers.flatMap { it.declarationCheckers.anonymousInitializerCheckers }.toSet()

          override val controlFlowAnalyserCheckers: Set<FirControlFlowChecker>
            get() = checkers.flatMap { it.declarationCheckers.controlFlowAnalyserCheckers }.toSet()

          override val propertyAccessorCheckers: Set<FirPropertyAccessorChecker>
            get() = checkers.flatMap { it.declarationCheckers.propertyAccessorCheckers }.toSet()

          override val typeAliasCheckers: Set<FirTypeAliasChecker>
            get() = checkers.flatMap { it.declarationCheckers.typeAliasCheckers }.toSet()

          override val backingFieldCheckers: Set<FirBackingFieldChecker>
            get() = checkers.flatMap { it.declarationCheckers.backingFieldCheckers }.toSet()

          override val basicDeclarationCheckers: Set<FirBasicDeclarationChecker>
            get() = checkers.flatMap { it.declarationCheckers.basicDeclarationCheckers }.toSet()

          override val callableDeclarationCheckers: Set<FirCallableDeclarationChecker>
            get() = checkers.flatMap { it.declarationCheckers.callableDeclarationCheckers }.toSet()

          override val classCheckers: Set<FirClassChecker>
            get() = checkers.flatMap { it.declarationCheckers.classCheckers }.toSet()

          override val classLikeCheckers: Set<FirClassLikeChecker>
            get() = checkers.flatMap { it.declarationCheckers.classLikeCheckers }.toSet()

          override val regularClassCheckers: Set<FirRegularClassChecker>
            get() = checkers.flatMap { it.declarationCheckers.regularClassCheckers }.toSet()

          override val valueParameterCheckers: Set<FirValueParameterChecker>
            get() = checkers.flatMap { it.declarationCheckers.valueParameterCheckers }.toSet()

          override val typeParameterCheckers: Set<FirTypeParameterChecker>
            get() = checkers.flatMap { it.declarationCheckers.typeParameterCheckers }.toSet()

          override val variableAssignmentCfaBasedCheckers:
              Set<AbstractFirPropertyInitializationChecker>
            get() =
                checkers
                    .flatMap { it.declarationCheckers.variableAssignmentCfaBasedCheckers }
                    .toSet()

          override val enumEntryCheckers: Set<FirEnumEntryChecker>
            get() = checkers.flatMap { it.declarationCheckers.enumEntryCheckers }.toSet()

          override val constructorCheckers: Set<FirConstructorChecker>
            get() = checkers.flatMap { it.declarationCheckers.constructorCheckers }.toSet()

          override val fileCheckers: Set<FirFileChecker>
            get() = checkers.flatMap { it.declarationCheckers.fileCheckers }.toSet()

          override val functionCheckers: Set<FirFunctionChecker>
            get() = checkers.flatMap { it.declarationCheckers.functionCheckers }.toSet()

          override val propertyCheckers: Set<FirPropertyChecker>
            get() = checkers.flatMap { it.declarationCheckers.propertyCheckers }.toSet()

          override val simpleFunctionCheckers: Set<FirSimpleFunctionChecker>
            get() = checkers.flatMap { it.declarationCheckers.simpleFunctionCheckers }.toSet()

          override val anonymousFunctionCheckers: Set<FirAnonymousFunctionChecker>
            get() = checkers.flatMap { it.declarationCheckers.anonymousFunctionCheckers }.toSet()
        }
}
