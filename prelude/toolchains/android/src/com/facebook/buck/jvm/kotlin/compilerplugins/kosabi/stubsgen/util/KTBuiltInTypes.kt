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

// created by parsing types contained in kotlin/core/builtins
// https://github.com/JetBrains/kotlin/tree/master/core/builtins
object PlainKTBuiltInTypes : FTQCollection {
  /**
   * Returns the simple names of all types in this collection.
   *
   * @return A collection of simple type names defined in this class
   */
  override fun all(): Collection<String> {
    return listOf(
        "Comparable",
        "CharSequence",
        "Iterator",
        "MutableIterator",
        "ListIterator",
        "MutableListIterator",
        "Iterable",
        "MutableIterable",
        "Collection",
        "MutableCollection",
        "List",
        "MutableList",
        "Set",
        "MutableSet",
        "Map",
        "MutableMap",
        "String",
        "Enum",
        "Nothing",
        "Array",
        "Any",
        "Throwable",
        "ByteArray",
        "CharArray",
        "ShortArray",
        "IntArray",
        "LongArray",
        "FloatArray",
        "DoubleArray",
        "BooleanArray",
        "Annotation",
        "Boolean",
        "Byte",
        "Short",
        "Int",
        "Long",
        "Float",
        "Double",
        "Number",
        "Char",
        "Unit",
        "Function",
        "AnnotationTarget",
        "AnnotationRetention",
        "Target",
        "Retention",
        "Repeatable",
        "MustBeDocumented",
        "Deprecated",
        "DeprecatedSinceKotlin",
        "ReplaceWith",
        "DeprecationLevel",
        "ExtensionFunctionType",
        "ContextFunctionTypeParams",
        "ParameterName",
        "Suppress",
        "UnsafeVariance",
        "SinceKotlin",
        "DslMarker",
        "PublishedApi",
    )
  }
}

object KTBuiltInTypes {
  val KTBuiltIn_Comparable: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "Comparable",
          )
      )
  val KTBuiltIn_CharSequence: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "CharSequence",
          )
      )
  val KTBuiltIn_Iterator: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "Iterator",
          )
      )
  val KTBuiltIn_MutableIterator: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "MutableIterator",
          )
      )
  val KTBuiltIn_ListIterator: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "ListIterator",
          )
      )
  val KTBuiltIn_MutableListIterator: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "MutableListIterator",
          )
      )
  val KTBuiltIn_Iterable: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "Iterable",
          )
      )
  val KTBuiltIn_MutableIterable: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "MutableIterable",
          )
      )
  val KTBuiltIn_Collection: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "Collection",
          )
      )
  val KTBuiltIn_MutableCollection: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "MutableCollection",
          )
      )
  val KTBuiltIn_List: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "List",
          )
      )
  val KTBuiltIn_MutableList: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "MutableList",
          )
      )
  val KTBuiltIn_Set: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "Set",
          )
      )
  val KTBuiltIn_MutableSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "MutableSet",
          )
      )
  val KTBuiltIn_Map: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "Map",
          )
      )
  val KTBuiltIn_MutableMap: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "MutableMap",
          )
      )
  val KTBuiltIn_String: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "String",
          )
      )
  val KTBuiltIn_Enum: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "Enum",
          )
      )
  val KTBuiltIn_Nothing: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "Nothing",
          )
      )
  val KTBuiltIn_Array: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "Array",
          )
      )
  val KTBuiltIn_Any: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "Any",
          )
      )
  val KTBuiltIn_Throwable: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "Throwable",
          )
      )
  val KTBuiltIn_ByteArray: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ByteArray",
          )
      )
  val KTBuiltIn_CharArray: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "CharArray",
          )
      )
  val KTBuiltIn_ShortArray: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ShortArray",
          )
      )
  val KTBuiltIn_IntArray: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "IntArray",
          )
      )
  val KTBuiltIn_LongArray: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "LongArray",
          )
      )
  val KTBuiltIn_FloatArray: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "FloatArray",
          )
      )
  val KTBuiltIn_DoubleArray: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "DoubleArray",
          )
      )
  val KTBuiltIn_BooleanArray: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "BooleanArray",
          )
      )
  val KTBuiltIn_Annotation: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "Annotation",
          )
      )
  val KTBuiltIn_Boolean: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "Boolean",
          )
      )
  val KTBuiltIn_Byte: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "Byte",
          )
      )
  val KTBuiltIn_Short: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "Short",
          )
      )
  val KTBuiltIn_Int: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "Int",
          )
      )
  val KTBuiltIn_Long: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "Long",
          )
      )
  val KTBuiltIn_Float: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "Float",
          )
      )
  val KTBuiltIn_Double: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "Double",
          )
      )
  val KTBuiltIn_Number: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "Number",
          )
      )
  val KTBuiltIn_Char: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "Char",
          )
      )
  val KTBuiltIn_Unit: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "Unit",
          )
      )
  val KTBuiltIn_Function: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "Function",
          )
      )
  val KTBuiltIn_AnnotationTarget: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "annotation",
              "AnnotationTarget",
          )
      )
  val KTBuiltIn_AnnotationRetention: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "annotation",
              "AnnotationRetention",
          )
      )
  val KTBuiltIn_Target: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "annotation",
              "Target",
          )
      )
  val KTBuiltIn_Retention: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "annotation",
              "Retention",
          )
      )
  val KTBuiltIn_Repeatable: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "annotation",
              "Repeatable",
          )
      )
  val KTBuiltIn_MustBeDocumented: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "annotation",
              "MustBeDocumented",
          )
      )
  val KTBuiltIn_Deprecated: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "Deprecated",
          )
      )
  val KTBuiltIn_DeprecatedSinceKotlin: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "DeprecatedSinceKotlin",
          )
      )
  val KTBuiltIn_ReplaceWith: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ReplaceWith",
          )
      )
  val KTBuiltIn_DeprecationLevel: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "DeprecationLevel",
          )
      )
  val KTBuiltIn_ExtensionFunctionType: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ExtensionFunctionType",
          )
      )
  val KTBuiltIn_ContextFunctionTypeParams: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ContextFunctionTypeParams",
          )
      )
  val KTBuiltIn_ParameterName: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ParameterName",
          )
      )
  val KTBuiltIn_Suppress: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "Suppress",
          )
      )
  val KTBuiltIn_UnsafeVariance: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "UnsafeVariance",
          )
      )
  val KTBuiltIn_SinceKotlin: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "SinceKotlin",
          )
      )
  val KTBuiltIn_DslMarker: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "DslMarker",
          )
      )
  val KTBuiltIn_PublishedApi: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "PublishedApi",
          )
      )
}
