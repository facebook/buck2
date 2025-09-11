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

// Kotlin type alias that's not from java.lang
// https://github.com/JetBrains/kotlin/blob/master/libraries/stdlib/jvm/runtime/kotlin/TypeAliases.kt
object PlainKTJavaTypeAlias : FTQCollection {
  /**
   * Returns all FullTypeQualifier objects in this collection.
   *
   * @return A collection of all FullTypeQualifier vals defined in this class
   */
  override fun all(): Collection<String> {
    return listOf(
        "NoSuchElementException",
        "ConcurrentModificationException",
        "Comparator",
        "RandomAccess",
        "ArrayList",
        "LinkedHashMap",
        "HashMap",
        "LinkedHashSet",
        "HashSet",
    )
  }
}

object KTJavaTypeAlias {
  val KTAlias_NoSuchElementException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "NoSuchElementException",
          )
      )
  val KTAlias_ConcurrentModificationException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ConcurrentModificationException",
          )
      )
  val KTAlias_Comparator: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "Comparator",
          )
      )
  val KTAlias_RandomAccess: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "RandomAccess",
          )
      )
  val KTAlias_ArrayList: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ArrayList",
          )
      )
  val KTAlias_LinkedHashMap: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "LinkedHashMap",
          )
      )
  val KTAlias_HashMap: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "HashMap",
          )
      )

  val KTAlias_LinkedHashSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "LinkedHashSet",
          )
      )
  val KTAlias_HashSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "HashSet",
          )
      )
}
