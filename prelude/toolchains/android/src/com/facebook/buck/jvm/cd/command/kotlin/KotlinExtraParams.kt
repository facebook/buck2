/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.cd.command.kotlin

import com.facebook.buck.core.filesystems.AbsPath
import com.facebook.buck.jvm.java.CompileToJarStepFactory
import com.facebook.buck.jvm.java.ResolvedJavacOptions
import com.google.common.collect.ImmutableList
import com.google.common.collect.ImmutableMap
import com.google.common.collect.ImmutableSortedSet
import java.util.Optional

data class KotlinExtraParams(
    val extraClassPaths: ImmutableList<AbsPath>,
    val standardLibraryClassPath: AbsPath,
    val annotationProcessingClassPath: AbsPath,
    val annotationProcessingTool: AnnotationProcessingTool,
    val extraKotlincArguments: ImmutableList<String>,
    val kotlinCompilerPlugins: ImmutableMap<AbsPath, ImmutableMap<String, String>>,
    val kosabiPluginOptions: ImmutableMap<String, AbsPath>,
    val kosabiJvmAbiGenEarlyTerminationMessagePrefix: Optional<String>,
    val friendPaths: ImmutableSortedSet<AbsPath>,
    val kotlinHomeLibraries: ImmutableList<AbsPath>,
    val resolvedJavacOptions: ResolvedJavacOptions,
    val jvmTarget: Optional<String>,
    val shouldUseJvmAbiGen: Boolean,
    val jvmAbiGenPlugin: Optional<AbsPath>,
    val shouldVerifySourceOnlyAbiConstraints: Boolean,
    val depTrackerPlugin: Optional<AbsPath>,
    val shouldKotlincRunViaBuildToolsApi: Boolean,
    val shouldKotlincRunIncrementally: Boolean,
    val shouldUseStandaloneKosabi: Boolean,
    val incrementalStateDir: Optional<AbsPath>,
    private val languageVersionString: String
) : CompileToJarStepFactory.ExtraParams {
  val kotlincWorkingDir: Optional<AbsPath> =
      incrementalStateDir.map { dir: AbsPath -> dir.resolve(KOTLINC_WORKING_DIR) }

  val jvmAbiGenWorkingDir: Optional<AbsPath> =
      incrementalStateDir.map { dir: AbsPath -> dir.resolve(KOTLINC_JVM_ABI_GEN_WORKING_DIR) }

  val languageVersion: LanguageVersion = LanguageVersion(languageVersionString)

  companion object {
    private val KOTLINC_WORKING_DIR: String = "kotlinc_working_dir"
    private val KOTLINC_JVM_ABI_GEN_WORKING_DIR: String = "jvm_abi_gen_working_dir"
  }
}
