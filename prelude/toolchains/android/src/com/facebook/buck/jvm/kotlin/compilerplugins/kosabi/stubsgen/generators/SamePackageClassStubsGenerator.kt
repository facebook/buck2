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

import com.facebook.kotlin.compilerplugins.kosabi.common.stub.model.KStub
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.util.PlainJavaLangTypes
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.util.PlainKTBuiltInTypes
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.util.PlainKTJavaTypeAlias
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.util.PlainKTStdlibTypes
import org.jetbrains.kotlin.psi.KtUserType
import org.jetbrains.kotlin.psi.psiUtil.getChildOfType

// Generate stubs for all unknown classes that are not declared from the module, nor from imported
// or class paths
// This is to handle cases like we have dependent module that use same package name as the current
// module. (Like a GraphQL model
// In this case explicit import for usage of those classes are not needed)
class SamePackageClassStubsGenerator : StubsGenerator {

  override fun generateStubs(context: GenerationContext) {

    val modulePkgName = context.packageName() ?: return
    val allKnownSymbols = mutableSetOf<String>()

    // Kotlin/Java built-in, stdlib types
    val sdkInternalTypes =
        setOf(
                PlainKTStdlibTypes,
                PlainKTBuiltInTypes,
                PlainKTJavaTypeAlias,
                PlainJavaLangTypes,
            )
            .flatMap { it.all() }

    allKnownSymbols.addAll(sdkInternalTypes)

    // Imported types, declared types, and auto-imported external types from classpaths
    // Only include auto-imported types (java.lang.*) from externalTypeReferences
    // to avoid name collisions with non-auto-imported types that have the same simple name.
    // Types like android.*, javax.*, etc. require explicit imports and should not be included here.
    val autoImportedExternalTypes = context.externalTypeReferences.filter { it.isAutoImported() }
    allKnownSymbols.addAll(
        (context.importedTypes +
                context.declaredTypes +
                autoImportedExternalTypes +
                context.fullQualifierTypes)
            .flatMap { it.segments + it.names }
    )

    // Alias & type parameter names
    allKnownSymbols.addAll(context.typeAliasSymbol + context.importAlias + context.parameterNames)

    // Annotations
    allKnownSymbols.addAll(
        context.annotationEntries
            .mapNotNull { it.typeReference?.getChildOfType<KtUserType>() }
            .mapNotNull { it.referencedName }
    )

    val maybeUnknownClasses =
        context.usedUserTypes
            .mapNotNull { userType -> userType.referencedName }
            .toSet()
            .filterNot { it -> allKnownSymbols.contains(it) }
            .filter { it -> it.first().isUpperCase() }

    maybeUnknownClasses.forEach { typeName ->
      if (context.stubsContainer.find(modulePkgName, typeName) == null) {
        context.stubsContainer.add(KStub(modulePkgName, typeName))
      }
    }
  }
}
