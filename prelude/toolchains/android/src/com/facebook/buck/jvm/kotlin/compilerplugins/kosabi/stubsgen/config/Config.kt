/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.kotlin.compilerplugins.kosabi.stubsgen.config

import com.facebook.kotlin.compilerplugins.kosabi.common.FullTypeQualifier

// This is a set of types that are known to be generated during KSP. This is a workaround to
// prevent adding stubs for the types which leads to redeclaration errors.
// KSP processor happens after additional sources, the type is generated later and it is too late
// to unregister the types after analysis stage.
val knownKSPGeneratedTypes: Set<FullTypeQualifier> =
    setOf(
        FullTypeQualifier(listOf("com", "facebook", "eventbus", "GlobalBusEventId")),
    )
