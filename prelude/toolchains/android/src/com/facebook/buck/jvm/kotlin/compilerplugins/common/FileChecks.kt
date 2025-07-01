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

// A file in these directories is either generated and must not be codemoded or is compiler plugin
const val COMPILATION_PATH: String = "/buck-out/"

// Path fragment indicating a file has been generated from one or more compiler plugins
const val COMPILER_PLUGIN_MID_FIX: String = "_kotlinc_plugin_generated__/"

// Note: If we expand beyond fbandroid, this will need to be keyed to which repo as in jAST
val GENERATED_FILE_PATHS: List<String> = listOf(COMPILATION_PATH, "/generated-graphql/", "/thrift/")

val NON_LOCAL_SRC_PATHS: List<String> = listOf("/third-party/")

val GENERATED_REGEX: Regex = Regex("${"@"}[Gg]enerated")

@JvmInline
value class SourcePath(val path: String) {

  fun isGeneratedPath(): Boolean = GENERATED_FILE_PATHS.any { it in path }

  fun isCompilerPluginGeneratedPath(): Boolean =
      COMPILATION_PATH in path && COMPILER_PLUGIN_MID_FIX in path

  // Will need to fix if we move out of fbandroid, and if kotlin compiler plugins change
  fun getRepositoryPathFromPluginPath(): String {
    if (!isCompilerPluginGeneratedPath()) {
      return path
    }

    return path
        .replace(Regex("/buck-out/[a-zA-Z0-9]+/[a-zA-Z0-9]+/fbandroid"), "/fbandroid")
        .replace(Regex("/__[a-zA-Z0-9]+_kotlinc_plugin_generated__/"), "/")
  }

  fun isThirdPartyPath(): Boolean = NON_LOCAL_SRC_PATHS.any { it in path }

  fun isGeneratedFile(ktFile: KtFile): Boolean {
    val isCodemodFilePath = isCompilerPluginGeneratedPath()
    val isGeneratedFilePath = if (isCodemodFilePath) false else isGeneratedPath()
    // This is sadly expensive due to getting the full string of the file, would love to just peek
    // at the top
    return if (isGeneratedFilePath) isGeneratedFilePath else GENERATED_REGEX in ktFile.text
  }
}
