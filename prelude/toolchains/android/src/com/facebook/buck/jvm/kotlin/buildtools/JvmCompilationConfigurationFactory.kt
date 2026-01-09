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
    private val kotlinCDLoggingContext: KotlinCDLoggingContext,
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
                            mode.kotlicWorkingDir.resolve("shrunk-classpath-snapshot.bin").toFile(),
                    ),
                options =
                    makeClasspathSnapshotBasedIncrementalCompilationConfiguration().apply {
                      setRootProjectDir(mode.rootProjectDir.toFile())
                      setBuildDir(mode.buildDir.toFile())
                      usePreciseCompilationResultsBackup(true)
                      keepIncrementalCompilationCachesInMemory(true)

                      val rebuildReason = mode.rebuildReason
                      if (rebuildReason != null) {
                        LOG.info(
                            "Non-incremental compilation will be performed: ${rebuildReason.message}"
                        )
                        kotlinCDLoggingContext.addExtras(
                            JvmCompilationConfigurationFactory::class.java.simpleName,
                            "Non-incremental compilation will be performed: ${rebuildReason.message}",
                        )
                        forceNonIncrementalMode(true)
                      }

                      when (mode.classpathChanges) {
                        is ClasspathChanges.Unknown -> {
                          LOG.info(
                              "Non-incremental compilation will be performed: classpath changes not available"
                          )
                          kotlinCDLoggingContext.addExtras(
                              JvmCompilationConfigurationFactory::class.java.simpleName,
                              "Non-incremental compilation will be performed: classpath changes not available",
                          )
                          forceNonIncrementalMode(true)
                        }
                        is ClasspathChanges.NoChanges -> {
                          assureNoClasspathSnapshotsChanges(true)
                        }
                        is ClasspathChanges.ToBeComputedByIncrementalCompiler -> {
                          // Classpath has additions or modifications only.
                          // The Kotlin incremental compiler can handle this case.
                        }
                        is ClasspathChanges.HasRemovals -> {
                          // Force non-incremental mode when classpath entries are removed.
                          // The Kotlin compiler's incremental compilation doesn't reliably detect
                          // that existing compiled code references classes from removed
                          // dependencies.
                          // See:
                          // https://fb.workplace.com/groups/2222954841208728/permalink/4171196826470000/
                          LOG.info(
                              "Non-incremental compilation will be performed: classpath removal detected"
                          )
                          kotlinCDLoggingContext.addExtras(
                              JvmCompilationConfigurationFactory::class.java.simpleName,
                              "Non-incremental compilation will be performed: classpath removal detected",
                          )
                          forceNonIncrementalMode(true)
                        }
                      }
                    },
            )
          }
        }
      }

  companion object {
    private val LOG: Logger = Logger.get(JvmCompilationConfigurationFactory::class.java)
  }
}
