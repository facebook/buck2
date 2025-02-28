/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.core.util.immutables.BuckStyleValueWithBuilder;
import com.google.common.collect.ImmutableList;
import java.io.File;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

/** Resolved JavacOptions used in {@link JavacPipelineState} */
@BuckStyleValueWithBuilder
public abstract class ResolvedJavacOptions {

  public abstract Optional<String> getBootclasspath();

  public abstract ImmutableList<RelPath> getBootclasspathList();

  public abstract JavacLanguageLevelOptions getLanguageLevelOptions();

  public abstract boolean isDebug();

  public abstract boolean isVerbose();

  public abstract JavacPluginParams getJavaAnnotationProcessorParams();

  public abstract JavacPluginParams getStandardJavacPluginParams();

  public abstract List<String> getExtraArguments();

  public boolean isJavaAnnotationProcessorParamsPresent() {
    return !getJavaAnnotationProcessorParams().isEmpty();
  }

  public ResolvedJavacOptions withJavaAnnotationProcessorParams(
      JavacPluginParams javaAnnotationProcessorParams) {
    if (getJavaAnnotationProcessorParams().equals(javaAnnotationProcessorParams)) {
      return this;
    }
    return ImmutableResolvedJavacOptions.builder()
        .from(this)
        .setJavaAnnotationProcessorParams(javaAnnotationProcessorParams)
        .build();
  }

  /** Creates {@link ResolvedJavacOptions} */
  public static ResolvedJavacOptions of(
      Optional<String> bootclasspath,
      ImmutableList<RelPath> bootclasspathList,
      JavacLanguageLevelOptions languageLevelOptions,
      boolean debug,
      boolean verbose,
      JavacPluginParams javaAnnotationProcessorParams,
      JavacPluginParams standardJavacPluginParams,
      List<String> extraArguments) {
    return ImmutableResolvedJavacOptions.builder()
        .setBootclasspath(bootclasspath)
        .setBootclasspathList(bootclasspathList)
        .setLanguageLevelOptions(languageLevelOptions)
        .setDebug(debug)
        .setVerbose(verbose)
        .setJavaAnnotationProcessorParams(javaAnnotationProcessorParams)
        .setStandardJavacPluginParams(standardJavacPluginParams)
        .setExtraArguments(extraArguments)
        .build();
  }

  /** Add options method */
  public static void appendOptionsTo(
      OptionsConsumer optionsConsumer,
      ResolvedJavacOptions resolvedJavacOptions,
      AbsPath rootCellRoot) {

    appendOptionsTo(
        rootCellRoot,
        optionsConsumer,
        getBootclasspathString(
            resolvedJavacOptions.getBootclasspath(), resolvedJavacOptions.getBootclasspathList()),
        resolvedJavacOptions.getLanguageLevelOptions(),
        resolvedJavacOptions.isDebug(),
        resolvedJavacOptions.isVerbose(),
        resolvedJavacOptions.getJavaAnnotationProcessorParams(),
        resolvedJavacOptions.getStandardJavacPluginParams(),
        resolvedJavacOptions.getExtraArguments());
  }

  private static void appendOptionsTo(
      AbsPath ruleCellRoot,
      OptionsConsumer optionsConsumer,
      Optional<String> bootclasspathString,
      JavacLanguageLevelOptions languageLevelOptions,
      boolean isDebug,
      boolean isVerbose,
      JavacPluginParams javaAnnotationProcessorParams,
      JavacPluginParams standardJavacPluginParams,
      List<String> extraArguments) {

    // Add some standard options.
    String sourceLevel = languageLevelOptions.getSourceLevelValue().getVersion();
    String targetLevel = languageLevelOptions.getTargetLevelValue().getVersion();
    optionsConsumer.addOptionValue("source", sourceLevel);
    optionsConsumer.addOptionValue("target", targetLevel);

    // Set the sourcepath to stop us reading source files out of jars by mistake.
    optionsConsumer.addOptionValue("sourcepath", "");

    if (isDebug) {
      optionsConsumer.addFlag("g");
    }

    if (isVerbose) {
      optionsConsumer.addFlag("verbose");
    }

    // Override the bootclasspath if Buck is building Java code for Android.
    bootclasspathString.ifPresent(
        bootclasspath -> optionsConsumer.addOptionValue("bootclasspath", bootclasspath));

    ImmutableList.Builder<ResolvedJavacPluginProperties> allPluginsBuilder =
        ImmutableList.builder();
    // Add annotation processors.
    if (!javaAnnotationProcessorParams.isEmpty()) {
      ImmutableList<ResolvedJavacPluginProperties> annotationProcessors =
          javaAnnotationProcessorParams.getPluginProperties();
      allPluginsBuilder.addAll(annotationProcessors);

      // Specify names of processors.
      optionsConsumer.addOptionValue(
          "processor",
          annotationProcessors.stream()
              .map(ResolvedJavacPluginProperties::getProcessorNames)
              .flatMap(Collection::stream)
              .collect(Collectors.joining(",")));

      // Add processor parameters.
      for (String parameter : javaAnnotationProcessorParams.getParameters()) {
        optionsConsumer.addFlag("A" + parameter);
      }
    } else {
      // Disable automatic annotation processor lookup
      optionsConsumer.addFlag("proc:none");
    }

    if (!standardJavacPluginParams.isEmpty()) {
      ImmutableList<ResolvedJavacPluginProperties> javacPlugins =
          standardJavacPluginParams.getPluginProperties();
      allPluginsBuilder.addAll(javacPlugins);

      for (ResolvedJavacPluginProperties properties : javacPlugins) {
        String name = properties.getProcessorNames().first();
        StringBuilder xplugin = new StringBuilder("Xplugin:");
        xplugin.append(name);
        for (String argument : properties.getArguments()) {
          xplugin.append(' ');
          xplugin.append(argument);
        }

        optionsConsumer.addFlag(xplugin.toString());

        // Add plugin's SourcePath params with RelPath's resolved relative to root
        for (Map.Entry<String, RelPath> sourcePathParam : properties.getPathParams().entrySet()) {
          optionsConsumer.addFlag(
              String.format(
                  "A%s=%s",
                  sourcePathParam.getKey(), ruleCellRoot.resolve(sourcePathParam.getValue())));
        }
      }

      // Add plugin parameters.
      optionsConsumer.addExtras(standardJavacPluginParams.getParameters());
    }

    // Specify classpath to include javac plugins and annotation processors.
    ImmutableList<ResolvedJavacPluginProperties> allPlugins = allPluginsBuilder.build();
    if (!allPlugins.isEmpty()) {
      optionsConsumer.addOptionValue(
          "processorpath",
          ResolvedJavacPluginProperties.getJoinedClasspath(allPlugins, ruleCellRoot));
    }

    // Add extra arguments.
    optionsConsumer.addExtras(extraArguments);
  }

  private static Optional<String> getBootclasspathString(
      Optional<String> bootclasspathOptional, ImmutableList<RelPath> bootclasspathList) {

    if (bootclasspathOptional.isPresent()) {
      return bootclasspathOptional;
    }

    if (bootclasspathList.isEmpty()) {
      return Optional.empty();
    }

    return Optional.of(
        bootclasspathList.stream()
            .map(Object::toString)
            .collect(Collectors.joining(File.pathSeparator)));
  }

  /** Validates classpath */
  public void validateClasspath(Function<String, Boolean> classpathChecker) throws IOException {
    Optional<String> bootclasspath = getBootclasspath();
    if (bootclasspath.isEmpty()) {
      return;
    }
    String bootClasspath = bootclasspath.get();
    try {
      if (!classpathChecker.apply(bootClasspath)) {
        throw new IOException(
            String.format("Bootstrap classpath %s contains no valid entries", bootClasspath));
      }
    } catch (UncheckedIOException e) {
      throw e.getCause();
    }
  }
}
