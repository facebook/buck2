/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.kotlin.compilerplugins.kosabi.common.stub.model

import com.facebook.kotlin.compilerplugins.kosabi.common.FullTypeQualifier

/**
 * [KClassType] does support some Java built-in types and customized class type. For example:
 * java.lang.String > kotlin.String, java.lang.Object > kotlin.Any, java.lang.annotation >
 * kotlin.Annotation ...
 *
 * More types info please refer to: https://kotlinlang.org/docs/java-interop.html#mapped-types
 */
class KClassType(
    private val fullQualifiedName: String,
    override val nullable: Boolean = false,
    override val generics: List<Type> = emptyList(),
) : Type {
  private val fullTypeQualifier: FullTypeQualifier

  init {
    val mappedFullQualifiedName =
        when (fullQualifiedName) {
          "java.lang.Object" -> "kotlin.Any"
          "java.lang.Cloneable" -> "kotlin.Cloneable"
          "java.lang.Comparable" -> "kotlin.Comparable"
          "java.lang.Enum" -> "kotlin.Enum"
          "java.lang.annotation.Annotation" -> "kotlin.Annotation"
          "java.lang.CharSequence" -> "kotlin.CharSequence"
          "java.lang.String" -> "kotlin.String"
          "java.lang.Number" -> "kotlin.Number"
          "java.lang.Throwable" -> "kotlin.Throwable"
          else -> fullQualifiedName
        }
    fullTypeQualifier = FullTypeQualifier(mappedFullQualifiedName.split("."))
  }

  override val names: List<String>
    get() = fullTypeQualifier.names

  override val pkg: List<String>
    get() = fullTypeQualifier.pkg
}
