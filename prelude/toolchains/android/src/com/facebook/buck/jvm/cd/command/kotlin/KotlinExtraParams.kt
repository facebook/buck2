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

import com.facebook.buck.core.filesystems.AbsPath
import com.facebook.buck.jvm.java.CompileToJarStepFactory
import com.facebook.buck.jvm.java.ResolvedJavacOptions
import com.google.common.collect.ImmutableList
import com.google.common.collect.ImmutableMap
import com.google.common.collect.ImmutableSortedSet
import java.util.Optional

data class KotlinExtraParams(
    val extraClassPaths: ImmutableList<AbsPath>,
    val extraClassPathSnapshots: ImmutableList<AbsPath>,
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
    val shouldKotlincRunIncrementally: Boolean,
    val incrementalStateDir: Optional<AbsPath>,
    val shouldKsp2RunIncrementally: Boolean,
    private val languageVersionString: String,
    val shouldKosabiJvmAbiGenUseK2: Boolean,
    val kotlinClassesDir: AbsPath,
) : CompileToJarStepFactory.ExtraParams {

  val shouldActionRunIncrementally: Boolean =
      shouldKotlincRunIncrementally || shouldKsp2RunIncrementally

  val kotlincWorkingDir: Optional<AbsPath> =
      incrementalStateDir.map { dir: AbsPath -> dir.resolve(KOTLINC_WORKING_DIR) }

  val ksp2CachesDir: Optional<AbsPath> =
      incrementalStateDir.map { dir: AbsPath -> dir.resolve(KSP2_CACHES_DIR) }

  val jvmAbiGenWorkingDir: Optional<AbsPath> =
      incrementalStateDir.map { dir: AbsPath -> dir.resolve(KOTLINC_JVM_ABI_GEN_WORKING_DIR) }

  val languageVersion: LanguageVersion = LanguageVersion(languageVersionString)

  companion object {
    private val KOTLINC_WORKING_DIR: String = "kotlinc_working_dir"
    private val KSP2_CACHES_DIR: String = "ksp2_caches_dir"
    private val KOTLINC_JVM_ABI_GEN_WORKING_DIR: String = "jvm_abi_gen_working_dir"
  }
}
