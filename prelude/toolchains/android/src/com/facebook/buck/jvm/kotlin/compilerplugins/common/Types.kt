/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.kotlin.compilerplugins.common

import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.kotlin.resolve.BindingContext

/**
 * Returns true if the compiler has done type analysis on any of the files in the list (which
 * implies that it has done it for all of them).
 */
fun hasTypesForAnyFiles(bindingContext: BindingContext, files: Collection<KtFile>): Boolean {
  return files.any { ktFile ->
    bindingContext.get(BindingContext.PACKAGE_TO_FILES, ktFile.packageFqName)?.contains(ktFile) ==
        true
  }
}
