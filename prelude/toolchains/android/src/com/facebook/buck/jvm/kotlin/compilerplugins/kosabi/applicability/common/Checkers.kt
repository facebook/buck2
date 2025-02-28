/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.kotlin.compilerplugins.kosabi.applicability.common

import com.facebook.kotlin.compilerplugins.kosabi.applicability.common.checkers.BlockImportListChecker
import com.facebook.kotlin.compilerplugins.kosabi.applicability.common.checkers.FeatureChecker
import com.facebook.kotlin.compilerplugins.kosabi.applicability.common.checkers.ImplicitConstantValueChecker
import com.facebook.kotlin.compilerplugins.kosabi.applicability.common.checkers.ImplicitImportChecker
import com.facebook.kotlin.compilerplugins.kosabi.applicability.common.checkers.ImportStarChecker
import com.facebook.kotlin.compilerplugins.kosabi.applicability.common.checkers.InheritanceOrderChecker
import com.facebook.kotlin.compilerplugins.kosabi.applicability.common.checkers.InlineChecker
import com.facebook.kotlin.compilerplugins.kosabi.applicability.common.checkers.NonExplicitTypeChecker
import com.facebook.kotlin.compilerplugins.kosabi.applicability.common.checkers.ProjectChecker
import com.facebook.kotlin.compilerplugins.kosabi.applicability.common.checkers.PureKotlinProjectChecker
import com.facebook.kotlin.compilerplugins.kosabi.applicability.common.checkers.SuperDelegationChecker
import com.facebook.kotlin.compilerplugins.kosabi.applicability.common.checkers.TypeAliasChecker

val PROJECT_CHECKERS: List<ProjectChecker> = listOf(PureKotlinProjectChecker)

val FEATURE_CHECKERS: List<FeatureChecker> =
    listOf(
        // [TypeAliasChecker] and [InheritanceOrderChecker] are the only checkers that need
        // [analysisCompleted] stage as they use bindingContext
        ImportStarChecker,
        InlineChecker,
        BlockImportListChecker,
        InheritanceOrderChecker,
        ImplicitConstantValueChecker,
        NonExplicitTypeChecker,
        SuperDelegationChecker,
        TypeAliasChecker,
        ImplicitImportChecker,
    )
