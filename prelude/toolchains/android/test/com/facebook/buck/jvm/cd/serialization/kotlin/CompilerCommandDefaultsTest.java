/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.cd.serialization.kotlin;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import com.facebook.buck.cd.model.java.AbiGenerationMode;
import com.facebook.buck.cd.model.java.ResolvedJavacOptions;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.jvm.cd.command.BaseJarCommand;
import com.facebook.buck.jvm.cd.serialization.java.JarParametersSerializer;
import com.facebook.buck.jvm.cd.serialization.java.ResolvedJavacOptionsSerializer;
import com.facebook.buck.jvm.java.Jsr199Javac.ResolvedJsr199Javac;
import java.util.Optional;
import java.util.logging.Level;
import org.junit.Test;

public class CompilerCommandDefaultsTest {
  @Test
  public void baseCommandUsesCompilerDefaults() {
    var model =
        com.facebook.buck.cd.model.java.BaseJarCommand.newBuilder()
            .setAbiGenerationMode(AbiGenerationMode.SOURCE_ONLY)
            .setTrackClassUsage(true)
            .setResolvedJavacOptions(javaOptions())
            .addCompileTimeClasspathPaths("dep.jar")
            .build();

    var command = BaseJarCommand.Companion.fromProto(model, Optional.of(RelPath.get("scratch")));

    assertEquals(AbiGenerationMode.CLASS, command.getAbiCompatibilityMode());
    assertEquals(AbiGenerationMode.SOURCE_ONLY, command.getAbiGenerationMode());
    assertTrue(command.getTrackClassUsage());
    assertEquals(RelPath.get("dep.jar"), command.getCompileTimeClasspathPaths().get(0));
    assertEquals(RelPath.get("buck-out/v2"), command.getBuckOut());
    assertTrue(command.getResolvedJavac() instanceof ResolvedJsr199Javac);
    assertTrue(command.getResolvedJavacOptions().getDebug());
  }

  @Test
  public void jarParametersUseFineLogging() {
    var model =
        com.facebook.buck.cd.model.java.JarParameters.newBuilder()
            .setJarPath("out.jar")
            .addEntriesToJar("classes")
            .build();

    var parameters = JarParametersSerializer.deserialize(model);

    assertEquals(Level.FINE, parameters.getDuplicatesLogLevel());
    assertEquals(RelPath.get("out.jar"), parameters.getJarPath());
    assertTrue(parameters.getEntriesToJar().contains(RelPath.get("classes")));
  }

  @Test
  public void annotationProcessorsKeepVariableFlagsWithoutPathParameters() {
    var model =
        javaOptions().toBuilder()
            .setJavaAnnotationProcessorParams(
                ResolvedJavacOptions.JavacPluginParams.newBuilder()
                    .addPluginProperties(
                        ResolvedJavacOptions.ResolvedJavacPluginProperties.newBuilder()
                            .setCanReuseClassLoader(true)
                            .setDoesNotAffectAbi(true)
                            .setSupportsAbiGenerationFromSource(true)
                            .setRunsOnJavaOnly(true)
                            .addProcessorNames("Processor")
                            .addClasspath("processor.jar")))
            .build();

    var processor =
        ResolvedJavacOptionsSerializer.deserialize(model)
            .getJavaAnnotationProcessorParams()
            .getPluginProperties()
            .get(0);

    assertTrue(processor.getCanReuseClassLoader());
    assertTrue(processor.getDoesNotAffectAbi());
    assertTrue(processor.getSupportAbiGenerationFromSource());
    assertTrue(processor.getRunsOnJavaOnly());
    assertTrue(processor.getPathParams().isEmpty());
    assertEquals(RelPath.get("processor.jar"), processor.getClasspath().get(0));
  }

  @Test
  public void kotlinParametersUseExactAbiTerminationMessage() {
    var model =
        com.facebook.buck.cd.model.kotlin.KotlinExtraParams.newBuilder()
            .setStandardLibraryClassPath("stdlib.jar")
            .setAnnotationProcessingClassPath("kapt.jar")
            .setKotlinClassesDir("classes")
            .setLanguageVersion("2.2")
            .build();

    var parameters = KotlinExtraParamsSerializer.deserialize(javaOptions(), model);

    assertEquals(
        Optional.of(
            "exception: java.lang.RuntimeException: Terminating compilation. We're done with ABI."),
        parameters.getKosabiJvmAbiGenEarlyTerminationMessagePrefix());
    assertTrue(parameters.getResolvedJavacOptions().getDebug());
  }

  private static ResolvedJavacOptions javaOptions() {
    return ResolvedJavacOptions.newBuilder()
        .setLanguageLevelOptions(
            ResolvedJavacOptions.JavacLanguageLevelOptions.newBuilder()
                .setSourceLevel("8")
                .setTargetLevel("8"))
        .build();
  }
}
