/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java

import com.facebook.buck.jvm.java.version.JavaVersion

data class JavacLanguageLevelOptions(val sourceLevel: String, val targetLevel: String) {
  val sourceLevelValue: JavaVersion
    get() = JavaVersion.toJavaLanguageVersion(sourceLevel)

  val targetLevelValue: JavaVersion
    get() = JavaVersion.toJavaLanguageVersion(targetLevel)

  companion object {
    // Default combined source and target level.
    const val TARGETED_JAVA_VERSION: String = "7"

    @JvmField
    val DEFAULT: JavacLanguageLevelOptions =
        JavacLanguageLevelOptions(TARGETED_JAVA_VERSION, TARGETED_JAVA_VERSION)
  }
}
