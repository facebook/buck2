/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.asm

import org.jetbrains.org.objectweb.asm.AnnotationVisitor
import org.jetbrains.org.objectweb.asm.ClassVisitor

@SuppressWarnings("PackageLocationMismatch")
internal class KotlinMetadataRemover(api: Int, cv: ClassVisitor) : ClassVisitor(api, cv) {
  override fun visitAnnotation(desc: String?, visible: Boolean): AnnotationVisitor? {
    if (desc == "Lkotlin/Metadata;") {
      return null
    }
    return super.visitAnnotation(desc, visible)
  }
}
