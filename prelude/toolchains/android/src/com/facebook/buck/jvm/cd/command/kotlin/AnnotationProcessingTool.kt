// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

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
