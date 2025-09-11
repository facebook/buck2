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

/**
 * The class to represent the Array type
 * *
 */
class KArrayType(
    val type: Type,
    override val nullable: Boolean = false,
    override val generics: List<Type> = emptyList(),
) : Type {

  override val names: List<String>
    get() =
        if (type is KArrayType) {
          type.type.names
        } else {
          type.names
        }

  override val pkg: List<String>
    get() =
        if (type is KArrayType) {
          type.type.pkg
        } else {
          type.pkg
        }
}
