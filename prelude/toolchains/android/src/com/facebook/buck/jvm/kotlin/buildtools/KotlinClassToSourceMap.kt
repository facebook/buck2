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

import com.facebook.buck.jvm.java.CompilerClassToSourceMap
import java.nio.file.Path
import org.jetbrains.kotlin.buildtools.api.KotlinLogger

internal class KotlinClassToSourceMap(
    private val output: Path,
    private val classesDir: Path,
    private val sourceRoot: Path,
) {
  private val mapping = CompilerClassToSourceMap()

  fun logger(delegate: KotlinLogger): KotlinLogger =
      object : KotlinLogger by delegate {
        override val isDebugEnabled: Boolean = true

        override fun debug(msg: String) {
          if (!recordOutput(msg)) delegate.debug(msg)
        }
      }

  fun recordOutput(message: String): Boolean {
    val report = message.removePrefix("output: ")
    if (!report.startsWith("Output:\n", ignoreCase = true)) return false
    val lines = report.lines()
    if (lines.size < 3 || lines[2] != "Sources:") return false
    val classFile = Path.of(lines[1]).toAbsolutePath().normalize()
    val root = classesDir.toAbsolutePath().normalize()
    if (classFile.startsWith(root) && classFile.toString().endsWith(".class")) {
      mapping.put(
          root.relativize(classFile).toString(),
          lines.drop(3).filter { it.isNotEmpty() }.map { Path.of(it) },
          sourceRoot,
      )
    }
    return true
  }

  fun write() {
    mapping.retainExistingOutputs(classesDir)
    mapping.write(output)
  }
}
