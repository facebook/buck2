/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.kotlin.compilerplugins.kosabi.common.stub.model

open class KPropertyStub(val name: String, val value: String?, val args: List<Type> = emptyList()) {
  var ret: Type = KType.UNIT
  var static: Boolean = false
}
