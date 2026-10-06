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
            // Deduplication is per file: the same text in two files can name two different types,
            // and each file's usage has to reach its own stub.
            .distinctBy { it.containingKtFile to it.text }

    // TODO: Do not apply for SDK classes
    // A usage resolves against the imports of ITS OWN file. Pooling every file's imports attributes
    // a simple name to whichever file imported it first, so two files importing different types of
    // the same name give one stub both arities and leave the other bare.
    val candidatesByFile =
        context.importedTypesByFile.mapValues { (_, imports) -> imports - context.declaredTypes }
    val pooledCandidates = context.importedTypes - context.declaredTypes
    for (genType in usedGenericTypes) {
      val candidates = candidatesByFile[genType.containingKtFile] ?: pooledCandidates
      val genFullQualifier = genType.calculateQualifierList()
      val imported = context.resolveImportedType(candidates, genFullQualifier.first())
      val imp: FullTypeQualifier
      val innerClassNames: List<String>
      if (imported != null) {
        imp = imported
        innerClassNames =
            if (genFullQualifier == imp.segments) emptyList()
            else (imp.names.drop(1) + genFullQualifier.drop(1))
      } else {
        // No import matches. A qualifier carrying its own package is written out in full; one
        // that carries none names a type of the using file's own package. Either way the type
        // arguments belong to the innermost name, and a trailing segment in this type slot names
        // a nested type whatever its case.
        val written = FullTypeQualifier(genFullQualifier).withMemberAsNestedName()
        val resolved =
            if (written.pkg.isEmpty()) {
              val filePkg = context.packageSegmentsOf(genType.containingKtFile)
              if (filePkg.isEmpty()) continue
              FullTypeQualifier(filePkg + genFullQualifier).withMemberAsNestedName()
            } else written
        imp = resolved
        innerClassNames = resolved.names.drop(1)
      }
      val name = imp.names
      // An all-caps simple name reads as a static-const member, so the qualifier carries no class
      // name to look up.
      if (name.isEmpty()) continue
      val pkg = imp.pkgAsString()

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
                .trimMargin(),
        )
      }
    }
  }
}
