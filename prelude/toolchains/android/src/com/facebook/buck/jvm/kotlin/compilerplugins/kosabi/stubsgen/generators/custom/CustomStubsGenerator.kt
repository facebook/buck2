/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.kotlin.compilerplugins.kosabi.stubsgen.generators.custom

import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.generators.GenerationContext
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.generators.StubsGenerator

/**
 * [CustomStubsGenerator] should be one of the last in the generation pipeline.
 *
 * Main goal is to stub additional classes that could be used during code generation. For example,
 * in Parcelize compiler plugin.
 */
class CustomStubsGenerator : StubsGenerator {
  private val customStubsGenerators: List<StubsGenerator> =
      listOf(ParcelizeStubsGenerator(), IgJsonParserAnnotationStubsGenerator())

  override fun generateStubs(context: GenerationContext) {
    customStubsGenerators.forEach { it.generateStubs(context) }
  }
}
