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

import com.facebook.buck.jvm.kotlin.cd.analytics.KotlinCDLoggingContext
import com.facebook.buck.jvm.kotlin.kotlinc.incremental.KotlincMode
import org.jetbrains.kotlin.buildtools.api.CompilationResult
import org.jetbrains.kotlin.buildtools.api.CompilationService
import org.jetbrains.kotlin.buildtools.api.ExperimentalBuildToolsApi
import org.jetbrains.kotlin.buildtools.api.KotlinLogger
import org.jetbrains.kotlin.buildtools.api.ProjectId

@OptIn(ExperimentalBuildToolsApi::class)
internal class KotlinCompilationService(
    private val compilationService: CompilationService,
    kotlinCDLoggingContext: KotlinCDLoggingContext
) {

  private val jvmCompilationConfigurationFactory =
      JvmCompilationConfigurationFactory(compilationService, kotlinCDLoggingContext)

  fun compile(
      projectId: ProjectId,
      arguments: List<String>,
      mode: KotlincMode,
      logger: KotlinLogger? = null
  ): CompilationResult {
    val compilerExecutionStrategyConfiguration =
        compilationService.makeCompilerExecutionStrategyConfiguration()

    val jvmCompilationConfiguration =
        jvmCompilationConfigurationFactory.create(mode).apply {
          if (logger != null) {
            useLogger(logger)
          }
        }

    return compilationService.compileJvm(
        projectId,
        compilerExecutionStrategyConfiguration,
        jvmCompilationConfiguration,
        emptyList(),
        arguments)
  }
}
