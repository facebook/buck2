/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.kotlin.ksp

import com.facebook.buck.core.build.execution.context.IsolatedExecutionContext
import com.facebook.buck.core.exceptions.HumanReadableException
import com.facebook.buck.core.filesystems.AbsPath
import com.facebook.buck.core.filesystems.RelPath
import com.facebook.buck.io.file.GlobPatternMatcher
import com.facebook.buck.jvm.cd.command.kotlin.LanguageVersion
import com.facebook.buck.jvm.core.BuildTargetValue
import com.facebook.buck.jvm.java.CompilerOutputPaths
import com.facebook.buck.jvm.kotlin.cd.analytics.KotlinCDAnalytics
import com.facebook.buck.jvm.kotlin.cd.analytics.KotlinCDLoggingContext
import com.facebook.buck.jvm.kotlin.cd.analytics.StepParam
import com.facebook.buck.jvm.kotlin.util.getExpandedSourcePaths
import com.facebook.buck.step.StepExecutionResult
import com.facebook.buck.step.StepExecutionResults
import com.facebook.buck.step.isolatedsteps.IsolatedStep
import com.facebook.buck.util.CapturingPrintStream
import com.google.common.base.Joiner
import com.google.common.collect.ImmutableList
import com.google.common.collect.ImmutableSortedSet
import com.google.common.collect.Iterables
import com.google.devtools.ksp.impl.KotlinSymbolProcessing
import com.google.devtools.ksp.processing.KSPJvmConfig
import com.google.devtools.ksp.processing.SymbolProcessorProvider
import java.io.File
import java.io.IOException
import java.io.PrintStream
import java.net.URLClassLoader
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.util.Optional
import java.util.ServiceLoader

class Ksp2Step(
    private val invokingRule: BuildTargetValue,
    private val outputPaths: CompilerOutputPaths,
    private val rootPath: AbsPath,
    private val shouldTrackClassUsage: Boolean,
    private val allClasspaths: ImmutableList<AbsPath>,
    private val kotlinPluginGeneratedOutFullPath: String,
    private val projectBaseDir: RelPath,
    private val annotationProcessorParams: ImmutableSortedSet<String>,
    private val sourceFilePaths: ImmutableSortedSet<RelPath>,
    private val kspDepFilePath: RelPath,
    private val moduleName: String,
    private val kspProcessorsClasspathList: List<String>,
    private val kspClassesOutput: RelPath,
    private val kspKotlinOutput: RelPath,
    private val kspJavaOutput: RelPath,
    private val kspCachesOutput: RelPath,
    private val kspOutput: RelPath,
    private val jvmTarget: Optional<String>,
    private val languageVersion: LanguageVersion,
    private val jvmDefaultMode: String,
    private val kotlinCDAnalytics: KotlinCDAnalytics
) : IsolatedStep {

  @Throws(IOException::class, InterruptedException::class)
  override fun executeIsolatedStep(context: IsolatedExecutionContext): StepExecutionResult {
    CapturingPrintStream().use { stderr ->
      try {
        val exitCode: KotlinSymbolProcessing.ExitCode = executeKsp2(stderr, context)
        kotlinCDAnalytics.log(KotlinCDLoggingContext(StepParam.KSP2, languageVersion, null))
        return when (exitCode) {
          KotlinSymbolProcessing.ExitCode.OK -> StepExecutionResults.SUCCESS
          KotlinSymbolProcessing.ExitCode.PROCESSING_ERROR ->
              StepExecutionResult(
                  StepExecutionResults.ERROR_EXIT_CODE,
                  Optional.of(stderr.getContentsAsString(StandardCharsets.UTF_8)))
        }
      } catch (e: LinkageError) {
        return StepExecutionResult(
            StepExecutionResults.ERROR_EXIT_CODE,
            Optional.of(
                "${stderr.getContentsAsString(StandardCharsets.UTF_8)}\n${e.stackTraceToString()}For URLClassLoader LinkError similar to P1626402598, try adding affected class to FilteringClassLoader's allowlist. See D63143327"))
      } catch (e: Throwable) {
        return StepExecutionResult(
            StepExecutionResults.ERROR_EXIT_CODE,
            Optional.of(
                "${stderr.getContentsAsString(StandardCharsets.UTF_8)}\n${e.stackTraceToString()}"))
      }
    }
  }

  private fun executeKsp2(
      stdErr: PrintStream,
      context: IsolatedExecutionContext,
  ): KotlinSymbolProcessing.ExitCode {
    val logger = BuckKsp2Logger(stdErr)

    // Load processors
    val processorClassloader: ClassLoader =
        URLClassLoader(
            kspProcessorsClasspathList.map { File(it).toURI().toURL() }.toTypedArray(),
            filteringClassLoader)
    val processorProviders =
        ServiceLoader.load(
                processorClassloader.loadClass(
                    "com.google.devtools.ksp.processing.SymbolProcessorProvider"),
                processorClassloader)
            .toList() as List<SymbolProcessorProvider>

    // Build processor options
    val apOptions = getApOptions()

    // Expand source paths
    val sourceFilePathsExpanded =
        getExpandedSourcePathsOrThrow(
            ruleCellRoot = context.ruleCellRoot,
            kotlinSourceFilePaths = sourceFilePaths,
            ignoredPathMatcher =
                if (invokingRule.isSourceOnlyAbi)
                    GlobPatternMatcher.of("**/kosabi_stub.android.**.kt")
                else null,
            workingDirectory = Optional.of(outputPaths.workingDirectory.getPath()),
            invokingRule = invokingRule,
            logger = logger,
        )
    val sourceFilePathsResolved = sourceFilePathsExpanded.map { rootPath.resolve(it).toFile() }

    // Build KSP config
    val kspConfig =
        KSPJvmConfig.Builder()
            .apply {
              // All configurations happen here. See [KSPConfig] for all available options.
              projectBaseDir = rootPath.resolve(this@Ksp2Step.projectBaseDir).toFile()
              classOutputDir = rootPath.resolve(kspClassesOutput).toFile()
              kotlinOutputDir = rootPath.resolve(kspKotlinOutput).toFile()
              javaOutputDir = rootPath.resolve(kspJavaOutput).toFile()
              resourceOutputDir = rootPath.resolve(kspClassesOutput).toFile()
              cachesDir = rootPath.resolve(kspCachesOutput).toFile()
              outputBaseDir = rootPath.resolve(kspOutput).toFile()
              processorOptions = apOptions
              moduleName = this@Ksp2Step.moduleName
              jvmTarget = this@Ksp2Step.jvmTarget.orElse("1.8")
              sourceRoots = sourceFilePathsResolved
              javaSourceRoots = sourceFilePathsResolved
              languageVersion = this@Ksp2Step.languageVersion.value
              apiVersion = this@Ksp2Step.languageVersion.value
              libraries = allClasspaths.map { it.toFile() }
              jvmDefaultMode = this@Ksp2Step.jvmDefaultMode
            }
            .build()

    logger.info(
        """Running KSP2 with
              |processors: ${processorProviders.joinToString(", ") { it::class.java.name }}
              |KSPJvmConfig: [
              |  projectBaseDir = ${kspConfig.projectBaseDir}
              |  classOutputDir = ${kspConfig.classOutputDir}
              |  kotlinOutputDir = ${kspConfig.kotlinOutputDir}
              |  javaOutputDir = ${kspConfig.javaOutputDir}
              |  resourceOutputDir = ${kspConfig.resourceOutputDir}
              |  cachesDir = ${kspConfig.cachesDir}
              |  outputBaseDir = ${kspConfig.outputBaseDir}
              |  processorOptions = [
              |    ${kspConfig.processorOptions.map { it.key + "=" + it.value }.joinToString(",\n    ")}]
              |  moduleName = ${kspConfig.moduleName}
              |  jvmTarget = ${kspConfig.jvmTarget}
              |  sourceRoots = ${kspConfig.sourceRoots}
              |  javaSourceRoots = ${kspConfig.javaSourceRoots}
              |  languageVersion = ${kspConfig.languageVersion}
              |  apiVersion = ${kspConfig.apiVersion}
              |  libraries = ${kspConfig.libraries}
              |  jvmDefaultMode = ${kspConfig.jvmDefaultMode}
              |]"""
            .trimMargin())
    // Run!
    val kotlinSymbolProcessing = KotlinSymbolProcessing(kspConfig, processorProviders, logger)
    return kotlinSymbolProcessing.execute()
  }

  private fun getApOptions(): MutableMap<String, String> {
    val apOptions = mutableMapOf<String, String>()

    // KSP needs the full classpath in order to resolve resources
    val allClasspath =
        Joiner.on(File.pathSeparator)
            .join(Iterables.transform(allClasspaths) { path: AbsPath -> path.getPath().toString() })
    apOptions["cp"] = allClasspath.replace(',', '-')

    if (shouldTrackClassUsage) {
      apOptions["fileAccessHistoryReportFile"] = rootPath.resolve(kspDepFilePath).toString()
    }

    if (invokingRule.isSourceOnlyAbi) {
      apOptions["com.facebook.buck.kotlin.generating_abi"] = "true"
    }

    for (param: String in annotationProcessorParams) {
      val paramAndValue = param.split("=".toRegex()).dropLastWhile { it.isEmpty() }.toTypedArray()
      check(paramAndValue.size == 2)
      apOptions[paramAndValue[0]] = paramAndValue[1]
    }

    apOptions["com.facebook.buck.kotlin.ksp_generated_out_path"] = kotlinPluginGeneratedOutFullPath
    return apOptions
  }

  override fun getIsolatedStepDescription(context: IsolatedExecutionContext): String {
    return "Running ksp2 for $moduleName"
  }

  override fun getShortName(): String {
    return "ksp2"
  }

  private fun getExpandedSourcePathsOrThrow(
      ruleCellRoot: AbsPath,
      kotlinSourceFilePaths: ImmutableSortedSet<RelPath>,
      ignoredPathMatcher: GlobPatternMatcher?,
      workingDirectory: Optional<Path>,
      invokingRule: BuildTargetValue,
      logger: BuckKsp2Logger
  ) =
      try {
        getExpandedSourcePaths(ruleCellRoot, kotlinSourceFilePaths, workingDirectory).filterNot {
            path: Path ->
          ignoredPathMatcher?.matches(path) ?: false
        }
      } catch (exception: IOException) {
        logger.exception(exception)
        throw HumanReadableException(
            "Unable to expand sources for ${invokingRule.fullyQualifiedName} into $workingDirectory")
      }

  companion object {
    private val filteringClassLoader =
        FilteringClassLoader(
            this::class.java.classLoader,
            ClassLoader.getPlatformClassLoader(),
            "com.google.devtools.ksp.",
            "kotlin.",
            "ksp.")
  }
}
