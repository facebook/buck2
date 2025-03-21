/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.kotlin.cd.analytics

import com.facebook.buck.jvm.cd.command.kotlin.LanguageVersion

/** A logging context designed to accumulate data relevant to KotlinCD operations. */
class KotlinCDLoggingContext(
    val step: StepParam,
    val languageVersion: LanguageVersion,
    val kotlincMode: KotlincModeParam?
) {

  val classpathChangesParam = (kotlincMode as? KotlincModeParam.Incremental)?.classpathChangesParam

  private val _extras: MutableMap<String, MutableList<String>> = mutableMapOf()
  val extras: Map<String, List<String>>
    get() = _extras

  fun addExtras(key: String, value: String) {
    _extras.computeIfAbsent(key) { mutableListOf() }.add(value)
  }
}
