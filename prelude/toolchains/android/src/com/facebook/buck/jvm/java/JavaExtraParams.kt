/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java

import com.facebook.buck.jvm.java.CompileToJarStepFactory.ExtraParams

/** Extra params instance used to create Java related compile build steps */
data class JavaExtraParams(
    val resolvedJavacOptions: ResolvedJavacOptions,
    val addAnnotationPath: Boolean
) : ExtraParams {
  companion object {
    @JvmStatic
    fun of(resolvedJavacOptions: ResolvedJavacOptions): JavaExtraParams {
      return JavaExtraParams(resolvedJavacOptions, true)
    }

    @JvmStatic
    fun of(
        resolvedJavacOptions: ResolvedJavacOptions,
        addAnnotationPath: Boolean
    ): JavaExtraParams {
      return JavaExtraParams(resolvedJavacOptions, addAnnotationPath)
    }
  }
}
