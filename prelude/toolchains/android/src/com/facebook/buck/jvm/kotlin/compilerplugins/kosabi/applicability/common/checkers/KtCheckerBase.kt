/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.kotlin.compilerplugins.kosabi.applicability.common.checkers

import org.jetbrains.kotlin.psi.KtElement
import org.jetbrains.kotlin.psi.KtVisitorVoid

abstract class KtCheckerBase : KtVisitorVoid() {
  abstract val violations: List<Violation>

  override fun visitKtElement(element: KtElement) {
    super.visitKtElement(element)
    element.acceptChildren(this)
  }
}
