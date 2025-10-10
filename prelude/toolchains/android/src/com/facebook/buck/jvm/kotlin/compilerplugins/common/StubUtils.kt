/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.kotlin.compilerplugins.common

import org.jetbrains.kotlin.backend.common.output.OutputFile
import org.jetbrains.kotlin.com.intellij.psi.PsiComment
import org.jetbrains.kotlin.psi.KtFile

// TODO: pass as a compiler plugin parameter
const val STUB_MARKUP = "// A stub @g" + "enerated by Kosabi"

// TODO: If [KtFile] doesn't have a package element then
//  [firstChild] will be an empty package element
//  Need to fix it
fun KtFile.isStub(): Boolean = firstChild is PsiComment && firstChild.text == STUB_MARKUP

fun OutputFile.isGeneratedFromStub(stubs: List<KtFile>): Boolean {
  // TODO(sergei): Do we want to cache this set?
  val stubsPaths: Set<String> = stubs.map { it.viewProvider.virtualFile.path }.toSet()
  val outPaths = sourceFiles.map { it.path }
  return stubsPaths.containsAll(outPaths)
}
