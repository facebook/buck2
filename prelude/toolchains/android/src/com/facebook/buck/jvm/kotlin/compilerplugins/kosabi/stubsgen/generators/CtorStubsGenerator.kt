/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.kotlin.compilerplugins.kosabi.stubsgen.generators

import com.facebook.kotlin.compilerplugins.kosabi.common.stub.model.KCtorStub
import com.facebook.kotlin.compilerplugins.kosabi.common.stub.model.KStub

class CtorStubsGenerator : StubsGenerator {

  override fun generateStubs(context: GenerationContext) {
    // TODO: Do not apply for SDK classes
    updateClassCtor(context.stubsContainer.all(), context)
  }

  private fun updateClassCtor(stubs: Collection<KStub>, context: GenerationContext) {
    for (stub in stubs) {
      if (stub.ctor == null) {
        updateCtorParams(stub, context)
      }
      updateClassCtor(stub.innerStubs, context)
    }
  }

  private fun updateCtorParams(stub: KStub, context: GenerationContext) {
    context.typeValueArgs[stub.name]?.let { stub.ctor = KCtorStub(it) }
  }
}
