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
import org.jetbrains.org.objectweb.asm.Opcodes

@SuppressWarnings("PackageLocationMismatch")
internal class InnerClassesCollectingVisitor : ClassVisitor(Opcodes.API_VERSION) {
  lateinit var ownInternalName: String
    private set

  private val myInnerClasses = arrayListOf<String>()
  val innerClasses: List<String>
    get() = myInnerClasses

  override fun visit(
      version: Int,
      access: Int,
      name: String,
      signature: String?,
      superName: String?,
      interfaces: Array<out String>?
  ) {
    super.visit(version, access, name, signature, superName, interfaces)
    ownInternalName = name
  }

  override fun visitInnerClass(name: String, outerName: String?, innerName: String?, access: Int) {
    super.visitInnerClass(name, outerName, innerName, access)
    myInnerClasses.add(name)
  }
}
