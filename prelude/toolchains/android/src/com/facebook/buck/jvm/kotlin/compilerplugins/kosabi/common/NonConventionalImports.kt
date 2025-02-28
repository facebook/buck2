/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.kotlin.compilerplugins.kosabi.common

object NonConventionalImports {
  /** [interfaces] is exclude list from Constant Value Naming convention in kosabi. */
  val interfaces: Set<List<String>> =
      setOf(
          listOf("javax", "microedition", "khronos", "egl", "EGL"),
          listOf("javax", "microedition", "khronos", "egl", "EGL10"),
          listOf("javax", "microedition", "khronos", "egl", "EGL11"),
          listOf("javax", "microedition", "khronos", "opengles", "GL"),
          listOf("javax", "microedition", "khronos", "opengles", "GL10"),
          listOf("javax", "microedition", "khronos", "opengles", "GL11"))
}
