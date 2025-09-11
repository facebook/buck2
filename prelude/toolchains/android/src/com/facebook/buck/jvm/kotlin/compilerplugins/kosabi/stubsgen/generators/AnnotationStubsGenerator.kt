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

import com.facebook.kotlin.compilerplugins.kosabi.common.Logger
import com.facebook.kotlin.compilerplugins.kosabi.common.filterDifferentOuterClassIn
import com.facebook.kotlin.compilerplugins.kosabi.common.outerClassOnlyQualifier
import com.facebook.kotlin.compilerplugins.kosabi.common.stub.model.KStub
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.util.calculateQualifierList
import org.jetbrains.kotlin.psi.KtUserType
import org.jetbrains.kotlin.psi.psiUtil.getChildOfType

/**
 * Tuned stub type to Annotation Class
 * 1. ClassDeclaration's AnnotationEntry
 * 2. (TODO) function parameters
 * 3. (TODO) primary constructor
 */
class AnnotationStubsGenerator : StubsGenerator {
  override fun generateStubs(context: GenerationContext) {
    val candidates =
        context.importedTypes.filterDifferentOuterClassIn(context.declaredTypes).filterNot {
          context.externalTypeReferences.contains(it.outerClassOnlyQualifier())
        }

    context.annotationEntries
        .mapNotNull { it.typeReference?.getChildOfType<KtUserType>() }
        .forEach { annotationType ->
          val genFullQualifier = annotationType.calculateQualifierList()
          val imp = candidates.find { it.names.last() == genFullQualifier.first() }

          if (imp != null) {
            val pkg = imp.pkgAsString()
            val name = imp.names.first()
            val inners: List<String> = imp.names.drop(1) + genFullQualifier.drop(1)

            val stub = context.stubsContainer.find(pkg, name, inners)
            if (stub != null) {
              stub.type = KStub.Type.ANNOTATION
            } else {
              Logger.log(
                  """
            |  [Warning] stub not found
            |    - name: $pkg:$name
            |    - inners: $inners
          """
                      .trimMargin()
              )
            }
          } else {

            Logger.log(
                """
          |  [Warning] ImportTypes not found
          |    - name: $genFullQualifier
          """
            )
          }
        }
  }
}
