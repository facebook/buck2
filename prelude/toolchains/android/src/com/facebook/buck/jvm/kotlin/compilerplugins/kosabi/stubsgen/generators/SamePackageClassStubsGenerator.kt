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
import com.facebook.kotlin.compilerplugins.kosabi.common.stub.model.KStub
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.util.JavaLangTypes
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.util.KTBuiltInTypes
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.util.KTJavaTypeAlias
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.util.KTStdlibTypes
import kotlin.reflect.full.companionObjectInstance
import kotlin.reflect.full.declaredMemberProperties
import org.jetbrains.kotlin.psi.KtUserType
import org.jetbrains.kotlin.psi.psiUtil.getChildOfType

// Generate stubs for all unknown classes that are not declared from the module, nor from imported
// or class paths
// This is to handle cases like we have dependent module that use same package name as the current
// module. (Like a GraphQL model
// In this case explicit import for usage of those classes are not needed)
class SamePackageClassStubsGenerator : StubsGenerator {

  override fun generateStubs(context: GenerationContext) {

    val modulePkgName = context.declaredTypes.firstOrNull()?.pkgAsString() ?: return
    val allKnownSymbols = mutableSetOf<String>()

    // Kotlin/Java built-in, stdlib types
    allKnownSymbols.addAll(
        listOfNotNull(
                KTStdlibTypes::class.companionObjectInstance,
                KTBuiltInTypes::class.companionObjectInstance,
                KTJavaTypeAlias::class.companionObjectInstance,
                JavaLangTypes::class.companionObjectInstance)
            .flatMap { cls ->
              cls::class
                  .declaredMemberProperties
                  .map { ktProperty -> ktProperty.getter.call(cls) }
                  .filterIsInstance<FullTypeQualifier>()
                  .map { it.names.first() }
            }
            .toSet())

    // Imported types, declared types, and external types from classpaths
    allKnownSymbols.addAll(
        (context.importedTypes +
                context.declaredTypes +
                context.externalTypeReferences +
                context.fullQualifierTypes)
            .flatMap { it.segments + it.names })

    // Alias & type parameter names
    allKnownSymbols.addAll(context.typeAliasSymbol + context.importAlias + context.parameterNames)

    // Annotations
    allKnownSymbols.addAll(
        context.annotationEntries
            .mapNotNull { it.typeReference?.getChildOfType<KtUserType>() }
            .mapNotNull { it.referencedName })

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
