/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.core

import com.facebook.buck.cd.model.java.BuildTargetValue.Type

/**
 * Build target representation used in java compilation. This class include only fields that used in
 * java compilation.
 */
data class BuildTargetValue(val type: Type, val fullyQualifiedName: String) {
  fun hasAbiJar(): Boolean {
    return isSourceAbi || isSourceOnlyAbi
  }

  val isLibraryJar: Boolean
    get() = this.type == Type.LIBRARY

  val isSourceAbi: Boolean
    get() = this.type == Type.SOURCE_ABI

  val isSourceOnlyAbi: Boolean
    get() = this.type == Type.SOURCE_ONLY_ABI

  override fun toString(): String {
    return this.fullyQualifiedName
  }
}
