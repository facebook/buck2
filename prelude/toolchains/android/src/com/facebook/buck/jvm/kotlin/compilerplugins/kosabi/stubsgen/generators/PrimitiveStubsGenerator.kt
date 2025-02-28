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
import com.facebook.kotlin.compilerplugins.kosabi.common.filterDifferentOuterClassIn
import com.facebook.kotlin.compilerplugins.kosabi.common.outerClassOnlyQualifier
import com.facebook.kotlin.compilerplugins.kosabi.common.stub.model.KStub

/**
 * [PrimitiveStubsGenerator] generates OPEN CLASSES stubs for all alien classes.
 *
 * Alien Class = Class outside of Project Context.
 */
class PrimitiveStubsGenerator : StubsGenerator {
  override fun generateStubs(context: GenerationContext) {
    val imports: Set<FullTypeQualifier> = context.importedTypes

    // We're creating simple stubs for everything we found not in classpaths
    imports
        .filterNot { it.pkgAsString().isSdkImport() }
        .filterDifferentOuterClassIn(context.declaredTypes)
        .filterNot { context.externalTypeReferences.contains(it.outerClassOnlyQualifier()) }
        .map { imp -> imp.pkgAsString() to imp.names.first() }
        .forEach { (pkg, name) ->
          if (name.first().isUpperCase() && context.stubsContainer.find(pkg, name) == null) {
            context.stubsContainer.add(KStub(pkg, name))
          }
        }
  }
}

private const val KOTLIN_STDLIB_PREFIX = "kotlin"
private const val JAVA_STDLIB_PREFIX = "java"

// kotlin.Unit - is an SDK class
// kotlinx.Parcelize - is not
private fun isKotlinSdk(pkg: String): Boolean =
    pkg == KOTLIN_STDLIB_PREFIX || pkg.startsWith("$KOTLIN_STDLIB_PREFIX.")

private fun isJavaSdk(pkg: String): Boolean =
    pkg == JAVA_STDLIB_PREFIX || pkg.startsWith("$JAVA_STDLIB_PREFIX.")

// TODO(Sergei): remove Kotlin/Java/Android SDK classes
private fun String.isSdkImport(): Boolean = isKotlinSdk(this) || isJavaSdk(this)

public fun FullTypeQualifier.isSdkQualifier(): Boolean =
    isKotlinSdk(segments.first()) || isJavaSdk(segments.first())
