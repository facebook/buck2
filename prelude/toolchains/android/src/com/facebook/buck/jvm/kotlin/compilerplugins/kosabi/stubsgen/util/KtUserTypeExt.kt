/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
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
