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
import org.jetbrains.kotlin.psi.KtUserType

/** [InnerClassStubsGenerator] should be after [CtorStubsGenerator] in the generation pipeline. */
class InterfaceStubsGenerator : StubsGenerator {
  override fun generateStubs(context: GenerationContext) {
    val candidates = context.importedTypes.filterDifferentOuterClassIn(context.declaredTypes)

    // Every stubbed bound of a multi-bound type parameter is emitted as an interface. Kotlin
    // permits at most one non-interface bound, so at most one bound of a group can be a class, and
    // the rules stacked with this one require that bound to be written first and to be on the
    // reduced classpath -- where it is the real type, is never stubbed, and holds the class slot on
    // its own. Interface is therefore the sound kind for every bound this pass can reach.
    //
    // A stub is born a class and carries no ClassKind, so leaving stubbed bounds alone corrupts a
    // group whose bounds are all off the classpath: kotlinc hands the single class slot to
    // whichever stub comes first and discards the other class-kinded bounds, which moves the
    // erasure of the parameter.
    //
    // A nested bound cannot be retyped either way -- its enclosing stub would stay a class whose
    // InnerClasses entry then describes an interface member, which javac rejects -- and the
    // decision is whole-group rather than per bound: on facecast's 36-bound clause, retyping the
    // group's OTHER tails still broke resolution of the nested bound at position 13, and dropping
    // any one earlier retype cleared it.
    val multiBoundStubs =
        context.multiBoundGroups
            .filterNot { group -> group.any { nested(candidates, it) } }
            .flatMap { group -> group.filter { isStubbed(context, candidates, it) } }

    for (iType in context.interfaceTypes + multiBoundStubs) {
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
                  .trimMargin(),
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
                .trimMargin(),
        )
      }
    }
  }

  // Retyping a nested stub would leave its enclosing stub a class whose InnerClasses entry
  // describes an interface member, and javac rejects that pair while completing the outer.
  private fun nested(candidates: Collection<FullTypeQualifier>, type: KtUserType): Boolean {
    val qualifierList = type.calculateQualifierList()
    val imp = candidates.find { it.names.last() == qualifierList.first() }
    return when {
      imp != null ->
          (imp.names.drop(1) + qualifierList.drop(1)).isNotEmpty() ||
              imp.pkgAsString().isNestedOwner()
      qualifierList.size > 1 ->
          FullTypeQualifier(qualifierList).let {
            it.names.drop(1).isNotEmpty() || it.pkgAsString().isNestedOwner()
          }
      else -> false
    }
  }

  // A stub exists exactly when the type is off the reduced classpath. A bound that is not stubbed
  // is the real type and already carries its own kind, so retyping it is neither needed nor safe.
  private fun isStubbed(
      context: GenerationContext,
      candidates: Collection<FullTypeQualifier>,
      first: KtUserType,
  ): Boolean {
    val qualifierList = first.calculateQualifierList()
    val imp = candidates.find { it.names.last() == qualifierList.first() }
    val pkg: String
    val name: String
    val inners: List<String>
    if (imp != null) {
      pkg = imp.pkgAsString()
      name = imp.names.first()
      inners = imp.names.drop(1) + qualifierList.drop(1)
    } else if (qualifierList.size > 1) {
      val full = FullTypeQualifier(qualifierList)
      pkg = full.pkgAsString()
      name = full.names.first()
      inners = full.names.drop(1)
    } else {
      // A single segment with no import is either declared in this module -- never stubbed -- or
      // a builtin, which is treated as stubbed because that only costs the retyping.
      return context.declaredTypes.none { it.names.last() == qualifierList.first() }
    }
    return context.stubsContainer.find(pkg, name, inners) != null
  }
}

// StubBytecodeRender.internalName treats an uppercase trailing package segment as an outer
// class, so a stub owned by one renders as `Outer$Inner`.
private fun String.isNestedOwner(): Boolean =
    substringAfterLast('.').firstOrNull()?.isUpperCase() == true
