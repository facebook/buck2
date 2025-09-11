/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.kotlin.compilerplugins.kosabi.stubsgen.stub.container

import com.facebook.kotlin.compilerplugins.kosabi.common.stub.model.KStub

interface StubsContainer {
  /**
   * @return
   * - null - if [StubsContainer] doesn't contain a relevant stub.
   * - A found [KStub] otherwise.
   *
   * Usage: find("com.facebook", "Outer", "Inner", "InnerInner")
   */
  fun find(pkg: String, className: String, innerClassNames: List<String> = emptyList()): KStub?

  /**
   * Adds a [stub] into [StubsContainer].
   *
   * Use [add] to add Outer classes stubs. To add an Inner class, [find] an Outer stub first.
   *
   * Methods expects that [StubsContainer] doesn't contain a [KStub] with the same qualifier.
   */
  fun add(stub: KStub)

  fun all(): Collection<KStub>
}
