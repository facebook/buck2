/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.java

import com.facebook.buck.cd.model.java.BuildTargetValue

/**
 * Value object that contains [CompilerOutputPaths] for library, source abi and source only abi
 * targets as well as library target fully qualified name.
 */
data class CompilerOutputPathsValue(
    val libraryTargetFullyQualifiedName: String,
    val libraryCompilerOutputPath: CompilerOutputPaths,
    val sourceAbiCompilerOutputPath: CompilerOutputPaths,
    val sourceOnlyAbiCompilerOutputPath: CompilerOutputPaths
) {
  /** Returns [CompilerOutputPaths] by given `type` */
  fun getByType(type: BuildTargetValue.Type): CompilerOutputPaths {
    return when (type) {
      BuildTargetValue.Type.LIBRARY -> libraryCompilerOutputPath
      BuildTargetValue.Type.SOURCE_ABI -> sourceAbiCompilerOutputPath
      BuildTargetValue.Type.SOURCE_ONLY_ABI -> sourceOnlyAbiCompilerOutputPath
      BuildTargetValue.Type.UNKNOWN,
      BuildTargetValue.Type.UNRECOGNIZED -> throw IllegalStateException("$type is not supported")

      else -> throw IllegalStateException("$type is not supported")
    }
  }

  companion object {
    /** Creates [CompilerOutputPathsValue] */
    @JvmStatic
    fun of(
        libraryTargetFullyQualifiedName: String,
        libraryCompilerOutputPath: CompilerOutputPaths,
        sourceAbiCompilerOutputPath: CompilerOutputPaths,
        sourceOnlyAbiCompilerOutputPath: CompilerOutputPaths
    ): CompilerOutputPathsValue {
      return CompilerOutputPathsValue(
          libraryTargetFullyQualifiedName,
          libraryCompilerOutputPath,
          sourceAbiCompilerOutputPath,
          sourceOnlyAbiCompilerOutputPath)
    }
  }
}
