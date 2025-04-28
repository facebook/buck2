/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.kotlin.compilerplugins.kosabi.stubsgen.util

import com.facebook.kotlin.compilerplugins.kosabi.common.FullTypeQualifier
import java.lang.RuntimeException
import org.jetbrains.kotlin.psi.KtImportDirective

fun KtImportDirective.toImportedClass(): FullTypeQualifier {
  // TODO: support multi-import cases
  // "import a.b.c.*

  // TODO: verify all cases where `importPath == null`
  // every case should be a separate `when` statement branch
  val importFq =
      importPath?.fqName
          ?: throw RuntimeException("For some reason importPath is null for[${this.text}].")

  val segments = importFq.pathSegments().map { it.asString() }
  return FullTypeQualifier(segments)
}
