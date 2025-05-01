/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.kotlin.compilerplugins.kosabi.stubsgen.generators

import com.facebook.kotlin.compilerplugins.kosabi.common.FullTypeQualifier
import com.facebook.kotlin.compilerplugins.kosabi.common.stub.model.KFunStub
import com.facebook.kotlin.compilerplugins.kosabi.common.stub.model.KStub
import java.util.Locale

// This name should be unique
const val TOP_LEVEL_DECLARATION_CLASS_LITERAL = "_kosabi_stub_top_level_declaration"

/**
 * [ClassLevelFunctionStubsGenerator] generates Class level function
 *
 * The current implementation only have the function name and body is "TODO"
 */
class ClassLevelFunctionStubsGenerator : StubsGenerator {

  override fun generateStubs(context: GenerationContext) {
    val classLevelFunctionImportsCandidate = context.importedDeclarations - context.declaredTypes
    val stubbedFuns = classLevelFunctionImportsCandidate.filter { it.isTopLevelDeclaration() }

    stubbedFuns
        .groupBy { it.pkg }
        .filterKeys { pkg -> !context.pkgsInClasspath.contains(pkg) }
        .forEach { (pkg, qualifiers) -> buildStub(pkg, qualifiers, context) }
  }

  private fun buildStub(
      pkg: List<String>,
      clazzList: List<FullTypeQualifier>,
      context: GenerationContext
  ) {
    val clazz = pkg.last() + TOP_LEVEL_DECLARATION_CLASS_LITERAL
    val packageName = pkg.joinToString(".")
    val className = createClassName(clazz)

    val kStub = KStub(packageName, className).apply { type = KStub.Type.TOP_LEVEL_DECLARATION }

    clazzList
        .mapNotNull { it.member }
        .forEach { kStub.funStubs.add(KFunStub(it, context.typeValueArgs[it] ?: 0)) }

    context.stubsContainer.add(kStub)
  }

  private fun createClassName(clazz: String): String {
    return capitalize(clazz)
  }

  private fun capitalize(name: String): String {
    return name.replaceFirstChar { it.titlecase(Locale.US) }
  }
}
