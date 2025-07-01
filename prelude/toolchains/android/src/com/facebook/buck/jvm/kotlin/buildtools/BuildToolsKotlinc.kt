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

import com.facebook.buck.core.build.execution.context.IsolatedExecutionContext
import com.facebook.buck.core.exceptions.HumanReadableException
import com.facebook.buck.core.filesystems.AbsPath
import com.facebook.buck.core.filesystems.RelPath
import com.facebook.buck.core.util.log.Logger
import com.facebook.buck.jvm.core.BuildTargetValue
import com.facebook.buck.jvm.kotlin.cd.analytics.KotlinCDLoggingContext
import com.facebook.buck.jvm.kotlin.kotlinc.Kotlinc
import com.facebook.buck.jvm.kotlin.kotlinc.incremental.KotlincMode
import com.facebook.buck.util.ClassLoaderCache
import com.google.common.collect.ImmutableList
import com.google.common.collect.ImmutableSortedSet
import java.io.IOException
import java.io.PrintStream
import java.nio.file.Path
import java.util.Optional
import java.util.UUID
import kotlin.io.path.extension
import org.jetbrains.kotlin.buildtools.api.CompilationService
import org.jetbrains.kotlin.buildtools.api.ExperimentalBuildToolsApi
import org.jetbrains.kotlin.buildtools.api.ProjectId

/**
 * Entry point for Kotlin compilation using BuildToolsApi. See:
 * https://github.com/JetBrains/kotlin/blob/master/compiler/build-tools/kotlin-build-tools-api/README.md
 */
@OptIn(ExperimentalBuildToolsApi::class)
class BuildToolsKotlinc : Kotlinc {

  override fun buildWithClasspath(
      context: IsolatedExecutionContext,
      invokingRule: BuildTargetValue,
      options: ImmutableList<String>,
      kotlinHomeLibraries: ImmutableList<AbsPath>,
      kotlinSourceFilePaths: ImmutableSortedSet<RelPath>,
      pathToSrcsList: Path,
      workingDirectory: Optional<Path>,
      ruleCellRoot: AbsPath,
      mode: KotlincMode,
      kotlinCDLoggingContext: KotlinCDLoggingContext
  ): Int {
    val compilerArgs =
        buildCompilerArgs(
            ruleCellRoot,
            kotlinSourceFilePaths,
            workingDirectory,
            invokingRule,
            options,
            kotlinCDLoggingContext)

    LOG.info(
        "[KotlinC Toolchain Build Step from for target:${invokingRule.fullyQualifiedName} type:${invokingRule.type}] " +
            "Running ${CompilationService::class.java.name} ${getIncrementalInfoMessage(mode)} " +
            "with arguments:[${compilerArgs.joinToString()}] ")

    val kotlinCompilationService =
        KotlinCompilationService(
            CompilationService.loadImplementation(
                context.classLoaderCache.getClassLoader(kotlinHomeLibraries)),
            kotlinCDLoggingContext)

    val result =
        kotlinCompilationService.compile(
            ProjectId.ProjectUUID(UUID.randomUUID()),
            compilerArgs,
            mode,
            BuckKotlinLogger(UncloseablePrintStream(context.stdErr), kotlinCDLoggingContext))

    return result.toExitCode.code
  }

  private fun getIncrementalInfoMessage(mode: KotlincMode) =
      when (mode) {
        is KotlincMode.Incremental -> "incrementally"
        is KotlincMode.NonIncremental -> "non incrementally"
        else -> throw UnsupportedOperationException("Unsupported KotlincMode")
      }

  private fun ClassLoaderCache.getClassLoader(kotlinHomeLibraries: List<AbsPath>): ClassLoader {
    val classPathURLs = kotlinHomeLibraries.map { absPath -> absPath.path.toUri().toURL() }
    return getClassLoaderForClassPath(
        SharedApiClassesClassLoaderProvider.sharedApiClassesClassLoader,
        ImmutableList.copyOf(classPathURLs))
  }

  private fun buildCompilerArgs(
      ruleCellRoot: AbsPath,
      kotlinSourceFilePaths: ImmutableSortedSet<RelPath>,
      workingDirectory: Optional<Path>,
      invokingRule: BuildTargetValue,
      options: List<String>,
      kotlinCDLoggingContext: KotlinCDLoggingContext
  ): List<String> {
    val expandedSources: ImmutableList<Path> =
        getExpandedSourcePathsOrThrow(
            ruleCellRoot, kotlinSourceFilePaths, workingDirectory, invokingRule)

    expandedSources
        .groupingBy { path -> path.extension }
        .eachCount()
        .forEach { (extension, count) ->
          kotlinCDLoggingContext.addExtras(
              BuildToolsKotlinc::class.java.simpleName, "Total count of $extension files: $count")
        }

    val resolvedExpandedSources =
        expandedSources.map { path -> ruleCellRoot.resolve(path).toString() }

    val isMultiPlatform = options.contains("-Xmulti-platform")

    return buildList {
      // Kotlin Multiplatform parameters pass-in source paths for each fragment
      // Similar to main source paths, we need to expand them as well
      if (isMultiPlatform) {
        val expandedOptions =
            getExpandedMultiPlatformSourcePathsOrThrow(options, resolvedExpandedSources)
        addAll(expandedOptions)
      } else {
        addAll(options)
      }

      addAll(resolvedExpandedSources)
    }
  }

  private fun getExpandedSourcePathsOrThrow(
      ruleCellRoot: AbsPath,
      kotlinSourceFilePaths: ImmutableSortedSet<RelPath>,
      workingDirectory: Optional<Path>,
      invokingRule: BuildTargetValue
  ) =
      try {
        getExpandedSourcePaths(ruleCellRoot, kotlinSourceFilePaths, workingDirectory)
      } catch (exception: IOException) {
        LOG.error(exception)
        throw HumanReadableException(
            "Unable to expand sources for ${invokingRule.fullyQualifiedName} into $workingDirectory")
      }

  private fun getExpandedMultiPlatformSourcePathsOrThrow(
      options: List<String>,
      allSources: List<String>
  ): List<String> =
      buildList() {
        options
            .filter { !it.isNullOrBlank() }
            .forEach { option ->
              if (option.startsWith("-Xfragment-sources")) {
                val (fragmentSourcesKey, unresolvedSources) = option.split("=")
                // construct `fragmentName:absolutePath` for each fragment source path
                // e.g. `-Xfragment-sources=android:/abs/path/A.kt,common:/abs/path/B.kt`
                val resolvedSourceOption =
                    unresolvedSources.split(",").map { fragmentSourcePath ->
                      val (fragmentName, fragmentPath) = fragmentSourcePath.split(":")
                      val fragmentSourceAbsPath =
                          allSources.firstOrNull { it.endsWith(fragmentPath) }

                      if (fragmentSourceAbsPath == null) {
                        throw HumanReadableException(
                            "Invalid fragment source path: $fragmentSourcePath")
                      }
                      "$fragmentName:$fragmentSourceAbsPath"
                    }
                add("$fragmentSourcesKey=${resolvedSourceOption.joinToString(",")}")
              } else {
                add(option)
              }
            }
      }

  override fun getDescription(
      options: ImmutableList<String>,
      kotlinSourceFilePaths: ImmutableSortedSet<RelPath>,
      pathToSrcsList: Path
  ): String = buildString {
    append("kotlinc ")
    append(options.joinToString(separator = " "))
    append(" @")
    append(pathToSrcsList)
  }

  override fun getShortName(): String = "kotlinc"

  companion object {
    private val LOG: Logger = Logger.get(BuildToolsKotlinc::class.java)
  }
}

private class UncloseablePrintStream(delegate: PrintStream) : PrintStream(delegate) {
  override fun close() {
    // ignore
  }
}
