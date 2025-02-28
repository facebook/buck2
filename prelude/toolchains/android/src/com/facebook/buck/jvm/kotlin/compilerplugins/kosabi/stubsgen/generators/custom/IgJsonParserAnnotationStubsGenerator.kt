/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.kotlin.compilerplugins.kosabi.stubsgen.generators.custom

import com.facebook.kotlin.compilerplugins.kosabi.common.stub.model.KCtorStub
import com.facebook.kotlin.compilerplugins.kosabi.common.stub.model.KStub
import com.facebook.kotlin.compilerplugins.kosabi.common.stub.model.asKType
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.generators.GenerationContext
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.generators.StubsGenerator
import kotlin.reflect.KClass

/** Remove this generator after T135299544 */
class IgJsonParserAnnotationStubsGenerator : StubsGenerator {
  override fun generateStubs(context: GenerationContext) {
    val jsonAdapter =
        context.stubsContainer.find("com.instagram.common.json.annotation", "JsonAdapter") ?: return

    with(jsonAdapter) {
      type = KStub.Type.ANNOTATION
      ctor =
          KCtorStub(
              listOf(
                  "adapterClass" to
                      KClass::class.asKType().parametriseWith(listOf(Any::class.asKType()))))
    }
  }
}
