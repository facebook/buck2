/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.kotlin.buildtools

import com.facebook.buck.core.filesystems.AbsPath
import kotlin.io.path.extension
import kotlin.io.path.readText
import kotlin.io.path.writeText
import org.intellij.lang.annotations.Language

internal class KotlinSourceFile(val absPath: AbsPath, @Language("kotlin") content: String) {

  init {
    require(absPath.hasKotlinFileExtension())

    absPath.path.writeText(content)
  }

  fun changeContent(from: String, to: String) {
    absPath.path.writeText(absPath.path.readText().replace(from, to))
  }
}

internal fun AbsPath.hasKotlinFileExtension() =
    listOf("kt", "kts").any { it.equals(path.extension, ignoreCase = true) }
