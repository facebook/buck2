/*
 * Portions Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

/*
 * Copyright 2010-2018 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package com.facebook.asm

import org.jetbrains.org.objectweb.asm.ClassVisitor

@SuppressWarnings("PackageLocationMismatch")
internal class FilterInnerClassesVisitor(
    private val innerClassesToFilter: Set<String>,
    api: Int,
    cv: ClassVisitor
) : ClassVisitor(api, cv) {
  override fun visitInnerClass(name: String, outerName: String?, innerName: String?, access: Int) {
    if (name in innerClassesToFilter) return

    super.visitInnerClass(name, outerName, innerName, access)
  }
}
