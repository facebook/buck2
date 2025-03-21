/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.kotlin.compilerplugins.kosabi.common.stub.model

/**
 * [KWildcardType] represents a wildcard type. It will be rendered into WildcardTypename.
 *
 * @param type type extends or super of the wildcard. <? extends T>, T is the type
 * @param bound UNBOUNDED - <?>, EXTENDS - <? extends T>, SUPER - <? super T>
 */
class KWildcardType(val type: Type, val bound: BoundType) : Type {
  enum class BoundType {
    UNBOUNDED,
    EXTENDS,
    SUPER
  }

  override val names: List<String> = type.names
  override val pkg: List<String> = type.pkg
  override val nullable: Boolean = type.nullable
  override val generics: List<Type> = emptyList()
}
