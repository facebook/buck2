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
import com.facebook.kotlin.compilerplugins.kosabi.common.filterDifferentOuterClassIn
import com.facebook.kotlin.compilerplugins.kosabi.common.stub.model.KStub
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.util.calculateQualifierList

/** [InnerClassStubsGenerator] should be after [CtorStubsGenerator] in the generation pipeline. */
class InterfaceStubsGenerator : StubsGenerator {
  override fun generateStubs(context: GenerationContext) {
    val candidates = context.importedTypes.filterDifferentOuterClassIn(context.declaredTypes)

    for (iType in context.interfaceTypes) {
      val qualifierList = iType.calculateQualifierList()
      val imp = candidates.find { it.names.last() == qualifierList.first() }
      var pkg: String
      var name: String
      var inners: List<String>

      if (imp == null) {
        // handle the case where full path is written in type ex. val x: a.b.A
        if (qualifierList.size > 1) {
          val fullTypeQualifier = FullTypeQualifier(qualifierList)
          pkg = fullTypeQualifier.pkgAsString()
          name = fullTypeQualifier.names.first()
          inners = fullTypeQualifier.names.drop(1)
        } else {
          Logger.log(
              """
        |  [Warning] ImportTypes not found
        |    - name: $qualifierList
      """
                  .trimMargin()
          )
          continue
        }
      } else {
        pkg = imp.pkgAsString()
        name = imp.names.first()
        inners = imp.names.drop(1) + qualifierList.drop(1)
      }

      val stub = context.stubsContainer.find(pkg, name, inners)
      if (stub != null) {
        stub.type = KStub.Type.INTERFACE
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
    }
  }
}
