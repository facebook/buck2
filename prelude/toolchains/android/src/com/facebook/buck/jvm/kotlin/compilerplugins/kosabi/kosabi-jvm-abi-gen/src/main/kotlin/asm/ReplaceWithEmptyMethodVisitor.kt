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

import org.jetbrains.kotlin.codegen.TransformationMethodVisitor
import org.jetbrains.org.objectweb.asm.MethodVisitor
import org.jetbrains.org.objectweb.asm.Opcodes
import org.jetbrains.org.objectweb.asm.tree.MethodNode

@SuppressWarnings("PackageLocationMismatch")
internal class ReplaceWithEmptyMethodVisitor(
    delegate: MethodVisitor,
    access: Int,
    name: String,
    desc: String,
    signature: String?,
    exceptions: Array<out String>?
) :
    TransformationMethodVisitor(
        delegate, access, name, desc, signature, exceptions, api = Opcodes.API_VERSION) {
  override fun performTransformations(methodNode: MethodNode) {
    methodNode.instructions.clear()
    methodNode.localVariables.clear()
    methodNode.tryCatchBlocks.clear()
  }
}
