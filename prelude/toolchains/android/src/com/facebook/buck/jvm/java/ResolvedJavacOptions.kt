/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.java

import com.facebook.buck.core.filesystems.AbsPath
import com.facebook.buck.core.filesystems.RelPath
import com.facebook.buck.jvm.java.version.JavaVersion
import com.google.common.collect.ImmutableList
import com.google.common.collect.ImmutableSortedSet
import java.io.File
import java.io.IOException
import java.io.UncheckedIOException
import java.util.Optional
import java.util.function.Function
import java.util.stream.Collectors

/** Resolved JavacOptions used in [JavacPipelineState] */
data class ResolvedJavacOptions(
    val bootclasspath: Optional<String>,
    val bootclasspathList: ImmutableList<RelPath>,
    val languageLevelOptions: JavacLanguageLevelOptions,
    val debug: Boolean,
    val verbose: Boolean,
    val javaAnnotationProcessorParams: JavacPluginParams,
    val standardJavacPluginParams: JavacPluginParams,
    val extraArguments: ImmutableList<String>,
    val systemImage: String? = null,
) {
  val isJavaAnnotationProcessorParamsPresent: Boolean
    get() = !javaAnnotationProcessorParams.isEmpty

  fun withJavaAnnotationProcessorParams(
      javaAnnotationProcessorParams: JavacPluginParams
  ): ResolvedJavacOptions {
    if (this.javaAnnotationProcessorParams == javaAnnotationProcessorParams) {
      return this
    }
    return ResolvedJavacOptions(
        bootclasspath,
        bootclasspathList,
        languageLevelOptions,
        debug,
        verbose,
        javaAnnotationProcessorParams,
        standardJavacPluginParams,
        extraArguments,
        systemImage)
  }

  /** Validates classpath */
  @Throws(IOException::class)
  fun validateClasspath(classpathChecker: Function<String, Boolean>) {
    if (bootclasspath.isEmpty) {
      return
    }
    val bootClasspath = bootclasspath.get()
    try {
      if (!classpathChecker.apply(bootClasspath)) {
        throw IOException(
            String.format("Bootstrap classpath %s contains no valid entries", bootClasspath))
      }
    } catch (e: UncheckedIOException) {
      throw e.cause!!
    }
  }

  companion object {

    /** Add options method */
    @JvmStatic
    fun appendOptionsTo(
        optionsConsumer: OptionsConsumer,
        resolvedJavacOptions: ResolvedJavacOptions,
        rootCellRoot: AbsPath
    ) {
      appendOptionsTo(
          rootCellRoot,
          optionsConsumer,
          getBootclasspathString(
              resolvedJavacOptions.bootclasspath, resolvedJavacOptions.bootclasspathList),
          resolvedJavacOptions.languageLevelOptions,
          resolvedJavacOptions.debug,
          resolvedJavacOptions.verbose,
          resolvedJavacOptions.javaAnnotationProcessorParams,
          resolvedJavacOptions.standardJavacPluginParams,
          resolvedJavacOptions.extraArguments)
    }

    private fun appendOptionsTo(
        ruleCellRoot: AbsPath,
        optionsConsumer: OptionsConsumer,
        bootclasspathString: Optional<String>,
        languageLevelOptions: JavacLanguageLevelOptions,
        isDebug: Boolean,
        isVerbose: Boolean,
        javaAnnotationProcessorParams: JavacPluginParams,
        standardJavacPluginParams: JavacPluginParams,
        extraArguments: List<String?>
    ) {
      // Add some standard options.

      val sourceLevel = languageLevelOptions.sourceLevelValue.version
      val targetLevel = languageLevelOptions.targetLevelValue.version
      optionsConsumer.addOptionValue("source", sourceLevel)
      optionsConsumer.addOptionValue("target", targetLevel)

      // Set the sourcepath to stop us reading source files out of jars by mistake.
      optionsConsumer.addOptionValue("sourcepath", "")

      if (isDebug) {
        optionsConsumer.addFlag("g")
      }

      if (isVerbose) {
        optionsConsumer.addFlag("verbose")
      }

      // Override the bootclasspath if Buck is building Java code for Android.
      if (languageLevelOptions.targetLevelValue <= JavaVersion.VERSION_8) {
        bootclasspathString.ifPresent { bootclasspath: String? ->
          optionsConsumer.addOptionValue("bootclasspath", bootclasspath)
        }
      }

      val allPluginsBuilder = ImmutableList.builder<ResolvedJavacPluginProperties>()
      // Add annotation processors.
      if (!javaAnnotationProcessorParams.isEmpty) {
        val annotationProcessors = javaAnnotationProcessorParams.pluginProperties
        allPluginsBuilder.addAll(annotationProcessors)

        // Specify names of processors.
        optionsConsumer.addOptionValue(
            "processor",
            annotationProcessors
                .stream()
                .map { obj: ResolvedJavacPluginProperties -> obj.processorNames }
                .flatMap { obj: ImmutableSortedSet<String> -> obj.stream() }
                .collect(Collectors.joining(",")))

        // Add processor parameters.
        for (parameter in javaAnnotationProcessorParams.parameters) {
          optionsConsumer.addFlag("A$parameter")
        }
      } else {
        // Disable automatic annotation processor lookup
        optionsConsumer.addFlag("proc:none")
      }

      if (!standardJavacPluginParams.isEmpty) {
        val javacPlugins = standardJavacPluginParams.pluginProperties
        allPluginsBuilder.addAll(javacPlugins)

        for (properties in javacPlugins) {
          val name = properties.processorNames.first()
          val xplugin = StringBuilder("Xplugin:")
          xplugin.append(name)
          for (argument in properties.arguments) {
            xplugin.append(' ')
            xplugin.append(argument)
          }

          optionsConsumer.addFlag(xplugin.toString())

          // Add plugin's SourcePath params with RelPath's resolved relative to root
          for ((key, value) in properties.pathParams) {
            optionsConsumer.addFlag(String.format("A%s=%s", key, ruleCellRoot.resolve(value)))
          }
        }

        // Add plugin parameters.
        optionsConsumer.addExtras(standardJavacPluginParams.parameters)
      }

      // Specify classpath to include javac plugins and annotation processors.
      val allPlugins = allPluginsBuilder.build()
      if (!allPlugins.isEmpty()) {
        optionsConsumer.addOptionValue(
            "processorpath",
            ResolvedJavacPluginProperties.getJoinedClasspath(allPlugins, ruleCellRoot))
      }

      // Add extra arguments.
      optionsConsumer.addExtras(extraArguments)
    }

    fun getBootclasspathString(
        bootclasspathOptional: Optional<String>,
        bootclasspathList: ImmutableList<RelPath>
    ): Optional<String> {
      if (bootclasspathOptional.isPresent) {
        return bootclasspathOptional
      }

      if (bootclasspathList.isEmpty()) {
        return Optional.empty()
      }

      return Optional.of(
          bootclasspathList
              .stream()
              .map(RelPath::toString)
              .collect(Collectors.joining(File.pathSeparator)))
    }
  }
}
