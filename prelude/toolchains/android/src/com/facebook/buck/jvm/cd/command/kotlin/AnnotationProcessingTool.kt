/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.cd.command.kotlin

enum class AnnotationProcessingTool {
  /**
   * Default tool for Kotlin modules. Allows to run Java annotation processors against Kotlin
   * sources while backporting it for Java sources too.
   */
  KAPT,

  /**
   * Works only against Java sources, Kotlin sources won't have access to generated classes at
   * compile time.
   */
  JAVAC,
}
