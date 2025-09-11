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

import org.jetbrains.kotlin.psi.KtUserType

/**
 * For a used type
 *
 * KtType(Outer.Mid.Inner) -> List(Outer, Mid, Inner)
 */
fun KtUserType.calculateQualifierList(): List<String> {
  val outer: MutableList<String> = mutableListOf()

  var type: KtUserType? = this
  while (type != null) {
    type.referencedName?.let { outer += it }
    type = type.qualifier
  }

  return outer.reversed()
}
