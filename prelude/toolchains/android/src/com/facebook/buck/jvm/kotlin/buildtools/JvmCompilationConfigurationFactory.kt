/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.kotlin.buildtools

import com.facebook.buck.core.util.log.Logger
import com.facebook.buck.jvm.kotlin.cd.analytics.KotlinCDLoggingContext
import com.facebook.buck.jvm.kotlin.kotlinc.incremental.ClasspathChanges
import com.facebook.buck.jvm.kotlin.kotlinc.incremental.KotlincMode
import com.facebook.buck.jvm.kotlin.kotlinc.incremental.KotlincMode.Incremental
import com.facebook.buck.jvm.kotlin.kotlinc.incremental.KotlincMode.NonIncremental
import org.jetbrains.kotlin.buildtools.api.CompilationService
import org.jetbrains.kotlin.buildtools.api.ExperimentalBuildToolsApi
import org.jetbrains.kotlin.buildtools.api.jvm.ClasspathSnapshotBasedIncrementalCompilationApproachParameters
import org.jetbrains.kotlin.buildtools.api.jvm.JvmCompilationConfiguration

@OptIn(ExperimentalBuildToolsApi::class)
internal class JvmCompilationConfigurationFactory(
    private val compilationService: CompilationService,
    private val kotlinCDLoggingContext: KotlinCDLoggingContext
) {

  fun create(mode: KotlincMode): JvmCompilationConfiguration =
      when (mode) {
        is NonIncremental -> {
          compilationService.makeJvmCompilationConfiguration()
        }
        is Incremental -> {
          compilationService.makeJvmCompilationConfiguration().apply {
            useIncrementalCompilation(
                workingDirectory = mode.kotlicWorkingDir.toFile(),
                sourcesChanges = mode.kotlinSourceChanges.toSourcesChanges(),
                approachParameters =
                    ClasspathSnapshotBasedIncrementalCompilationApproachParameters(
                        newClasspathSnapshotFiles = mode.classpathChanges.classpathSnapshotFiles,
                        shrunkClasspathSnapshot =
                            mode.kotlicWorkingDir
                                .resolve("shrunk-classpath-snapshot.bin")
                                .toFile()),
                options =
                    makeClasspathSnapshotBasedIncrementalCompilationConfiguration().apply {
                      setRootProjectDir(mode.rootProjectDir.toFile())
                      setBuildDir(mode.buildDir.toFile())
                      usePreciseCompilationResultsBackup(true)
                      keepIncrementalCompilationCachesInMemory(true)

                      if (mode.kotlinClassUsageFile.toFile().exists().not()) {
                        LOG.info(
                            "Non-incremental compilation will be performed: ${mode.kotlinClassUsageFile.fileName} not found")
                        kotlinCDLoggingContext.addExtras(
                            JvmCompilationConfigurationFactory::class.java.simpleName,
                            "Non-incremental compilation will be performed: ${mode.kotlinClassUsageFile.fileName} not found")
                        forceNonIncrementalMode(true)
                      }

                      // targets with abi_generation_mode = "source_only" do not use jvm-abi-gen for
                      // class abi generation so the directory is never created
                      val jvmAbiGenWorkingDir = mode.jvmAbiGenWorkingDir
                      if (jvmAbiGenWorkingDir != null &&
                          jvmAbiGenWorkingDir.toFile().exists().not()) {
                        LOG.info(
                            "Non-incremental compilation will be performed: ${jvmAbiGenWorkingDir.fileName} not found")
                        kotlinCDLoggingContext.addExtras(
                            JvmCompilationConfigurationFactory::class.java.simpleName,
                            "Non-incremental compilation will be performed: ${jvmAbiGenWorkingDir.fileName} not found")
                        forceNonIncrementalMode(true)
                      }

                      when (mode.classpathChanges) {
                        is ClasspathChanges.Unknown -> {
                          LOG.info(
                              "Non-incremental compilation will be performed: classpath changes not available")
                          kotlinCDLoggingContext.addExtras(
                              JvmCompilationConfigurationFactory::class.java.simpleName,
                              "Non-incremental compilation will be performed: classpath changes not available")
                          forceNonIncrementalMode(true)
                        }
                        is ClasspathChanges.NoChanges -> {
                          assureNoClasspathSnapshotsChanges(true)
                        }
                        else -> {}
                      }
                    })
          }
        }
      }

  companion object {
    private val LOG: Logger = Logger.get(JvmCompilationConfigurationFactory::class.java)
  }
}
