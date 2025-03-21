/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.kotlin.compilerplugins.kosabi.stubsgen.util

import com.facebook.kotlin.compilerplugins.kosabi.common.FullTypeQualifier

// Kotlin type alias that's not from java.lang
// https://github.com/JetBrains/kotlin/blob/master/libraries/stdlib/jvm/runtime/kotlin/TypeAliases.kt
class KTJavaTypeAlias private constructor() {
  companion object {
    val KTAlias_NoSuchElementException: FullTypeQualifier =
        FullTypeQualifier(
            listOf(
                "kotlin",
                "NoSuchElementException",
            ))
    val KTAlias_ConcurrentModificationException: FullTypeQualifier =
        FullTypeQualifier(
            listOf(
                "kotlin",
                "ConcurrentModificationException",
            ))
    val KTAlias_Comparator: FullTypeQualifier =
        FullTypeQualifier(
            listOf(
                "kotlin",
                "Comparator",
            ))
    val KTAlias_RandomAccess: FullTypeQualifier =
        FullTypeQualifier(
            listOf(
                "kotlin",
                "RandomAccess",
            ))
    val KTAlias_ArrayList: FullTypeQualifier =
        FullTypeQualifier(
            listOf(
                "kotlin",
                "ArrayList",
            ))
    val KTAlias_LinkedHashMap: FullTypeQualifier =
        FullTypeQualifier(
            listOf(
                "kotlin",
                "LinkedHashMap",
            ))
    val KTAlias_HashMap: FullTypeQualifier =
        FullTypeQualifier(
            listOf(
                "kotlin",
                "HashMap",
            ))

    val KTAlias_LinkedHashSet: FullTypeQualifier =
        FullTypeQualifier(
            listOf(
                "kotlin",
                "LinkedHashSet",
            ))
    val KTAlias_HashSet: FullTypeQualifier =
        FullTypeQualifier(
            listOf(
                "kotlin",
                "HashSet",
            ))
  }
}
