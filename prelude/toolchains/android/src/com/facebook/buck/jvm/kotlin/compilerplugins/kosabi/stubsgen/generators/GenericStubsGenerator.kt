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

import com.facebook.kotlin.compilerplugins.kosabi.common.FullTypeQualifier
import com.facebook.kotlin.compilerplugins.kosabi.common.Logger
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.util.calculateQualifierList
import org.jetbrains.kotlin.psi.KtTypeArgumentList
import org.jetbrains.kotlin.psi.KtUserType
import org.jetbrains.kotlin.psi.psiUtil.anyDescendantOfType
import org.jetbrains.kotlin.psi.psiUtil.collectDescendantsOfType

class GenericStubsGenerator : StubsGenerator {
  override fun generateStubs(context: GenerationContext) {
    val usedGenericTypes: List<KtUserType> =
        context.projectFiles
            .flatMap {
              // find all the types in the project files
              it.collectDescendantsOfType<KtUserType>()
            }
            // check if this is generic one
            .filter { it.anyDescendantOfType<KtTypeArgumentList>() }
            .distinctBy { it.text }

    // TODO: Do not apply for SDK classes
    val candidates = context.importedTypes - context.declaredTypes

    for (genType in usedGenericTypes) {
      val genFullQualifier = genType.calculateQualifierList()
      val imp =
          candidates.find { it.names.last() == genFullQualifier.first() }
              ?: if (genFullQualifier.size > 1) FullTypeQualifier(genFullQualifier) else continue
      val pkg = imp.pkgAsString()
      val name = imp.names
      val innerClassNames =
          if (genFullQualifier == imp.segments) emptyList<String>()
          else (name.drop(1) + genFullQualifier.drop(1))

      val stub =
          context.stubsContainer.find(
              pkg,
              name.first(),
              // Case 1:
              // name = com.A.B
              // genFullQualifier = B
              // Case 2:
              // name = com.A
              // genFullQualifier = A.B
              innerClassNames,
          )

      if (stub != null) {
        stub.genericTypes = genType.typeArguments.size
      } else {
        Logger.log(
            """
          |  [Warning] stub not found
          |    - name: $pkg:${name.first()}
          |    - inner class names: $innerClassNames
        """
                .trimMargin()
        )
      }
    }
  }
}
