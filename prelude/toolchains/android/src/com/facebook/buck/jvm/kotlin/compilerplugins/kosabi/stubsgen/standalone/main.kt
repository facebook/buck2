/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.kotlin.compilerplugins.kosabi.stubsgen.standalone

import java.io.File

fun main(args: Array<String>) {
  val config = parseKosabiConfig(args)
  println("Configuration: $config")
}

fun parseKosabiConfig(args: Array<String>): KosabiConfig? {
  var sourceRoots: List<File> = emptyList()
  var classpath: List<File> = emptyList()
  var outputDir: File? = null
  var logEnabled = false

  var i = 0
  while (i < args.size) {
    when (args[i]) {
      "--sourceRoots" -> {
        i++
        sourceRoots = args[i].split(",").map { File(it) }
      }

      "--classpath" -> {
        i++
        classpath = args[i].split(",").map { File(it) }
      }

      "--outputDir" -> {
        i++
        outputDir = File(args[i])
      }

      "--logEnabled" -> {
        i++
        logEnabled = args[i].toBoolean()
      }
    }
    i++
  }
  return if (outputDir == null) {
    null
  } else
      KosabiConfig(
          sourceRoots = sourceRoots,
          classpath = classpath,
          outputDir = outputDir,
          logEnabled = logEnabled,
      )
}
